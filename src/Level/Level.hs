{-# LANGUAGE NamedFieldPuns              #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE ViewPatterns                #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Level.Level
  ( Piece (..)
  , Hook (..)
  , Level (..)
  , Door (..)
  , levels
  , updateLevel
  , drawLine
  ) where

import           Actor.Constants
import qualified Data.Foldable as F
import qualified Data.Map as M
import           Data.Tiled
import           Game.Sequoia
import           Game.Sequoia.Color
import           Linear.Vector
import           Math
import           Object
import           ObjectMap
import qualified Types as T
import           Types hiding (Object (..))

isDeath :: Zone -> Bool
isDeath (Death _) = True
isDeath _         = False

isNoBoost :: Zone -> Bool
isNoBoost (NoBoost _) = True
isNoBoost _           = False

isDoor :: Zone -> Bool
isDoor (DoorZ _) = True
isDoor _         = False

getRect :: Zone -> Rect
getRect (Death   r) = r
getRect (NoBoost r) = r
getRect (DoorZ (Door r _)) = r

getDoor :: Zone -> Maybe Door
getDoor (DoorZ door) = Just door
getDoor _ = Nothing

drawLine :: Piece -> Form
drawLine (Wall (Line pos size) c _)
  = move pos
  . traced (solid c)
  . segment (0,0)
  $ unpackV2 size

levels :: [(String, Level)]
levels = zip names
       $ map (\x -> parseLayers (importScale *^ V2 (fromIntegral $ mapWidth x * mapTileWidth x) (fromIntegral $ mapHeight x * mapTileHeight x))
                                (fmap getTileset . listToMaybe $ mapTilesets x)
                                (mapLayers x))
       . unsafePerformIO
       . sequence
       . map (\n -> loadMapFile $ "level/" ++ n ++ ".tmx")
       $ names
  where names = [ "test1"
                , "test2"
                , "test3"
                , "test4"
                ]
{-# NOINLINE levels #-}

getTileset :: Tileset -> (Tileset, FilePath)
getTileset x = (x, ("level/" ++) . iSource . head $ tsImages x)

parseLayers :: V2 -> Maybe (Tileset, FilePath) -> [Layer] -> Level
parseLayers size tileset ls =
  let dz =  map (getRect) $ getZones isDeath
      nbz = map (getRect) $ getZones isNoBoost
      doors = getZones isDoor
   in Level
      { levelGeometry = parseCollision green $ getLayer "collision"
      , playerSpawn  = spawn
      , deathZones   = dz
      , noBoostZones = nbz
      , targets      = levelHooks
      , _objects     = levelObjects
      , doors = mapMaybe getDoor doors
      , forms = fmap targetForm levelHooks
             ++ zonesToForm dz  red
             ++ zonesToForm nbz cyan
             ++ zonesToForm (map getRect doors) purple
             ++ tiledata
      , levelSize = size
      , _destructableGeometry = M.fromListWith (++)
                              . fmap (\x -> (pieceGroup x, pure x))
                              . parseCollision red
                              $ getLayer "destructable"
      }
  where (spawn, zones) = parseMeta $ getLayer "meta"
        levelHooks   = parseHooks $ getLayer "targets"
        levelObjects = parseObjects $ getLayer "objects"
        tiledata     = parseTileset tileset $ getLayer "tiles"
        getLayer name  = listToMaybe $ filter ((== name) . layerName) ls
        getZones f = filter f zones
        zonesToForm zs c = map (mkForm c) zs
        targetForm (Hook pos) = move pos
                              . scale 0.2
                              . move (- V2 90 99)
                              . toForm
                              $ image "assets/hook.png"
        mkForm c (Rect pos size) = move (pos + size ^* 0.5)
                                 . outlined (solid c)
                                 . uncurry rect
                                 $ unpackV2 size


parseTileset :: Maybe (Tileset, FilePath) -> Maybe Layer -> [Form]
parseTileset (Just (ts, fp)) (Just Layer {layerData}) = fmap toTile $ M.toList layerData
  where
    toTile :: ((Int, Int), Tile) -> Form
    toTile ((x, y), (tileGid -> t))
      = move (V2 (fromIntegral x) (fromIntegral y) ^* (16 * importScale))
      . group
      . pure
      . scale importScale
      . toForm
      $ croppedImage (getCrop t) fp

    getCrop :: Word32 -> Crop
    getCrop (subtract 1 . fromIntegral -> g) =
      Crop ((g `mod` stride) * 16) ((g `div` stride) * 16) 16 16

    stride = let img = head $ tsImages ts
              in iWidth img `div` 16

parseTileset _ _ = []


getPosOfObj :: Object -> V2
getPosOfObj Object{objectX = x, objectY = y} = scaleDouble x y

scaleDouble :: Double -> Double -> V2
scaleDouble x y = V2 (importScale * x) (importScale * y)

parseMeta :: Maybe Layer -> (V2, [Zone])
parseMeta (Just ObjectLayer{layerObjects = objs}) =
    (spawn, deaths ++ noboosts ++ doors)
  where spawn = maybe (V2 0 0) getPosOfObj . listToMaybe $ getObjs "spawn"
        deaths = getZone Death "death"
        noboosts = getZone NoBoost "noboost"
        doors = map (\d -> DoorZ
                         . Door (toRect d)
                         . fromJust
                         . lookup "to"
                         $ objectProperties d) $ getObjs "door"

        getObjs t = filter (maybe False (== t) . objectType) objs
        toRect obj@(Object{objectWidth = w, objectHeight = h}) =
            Rect (getPosOfObj obj) $ scaleDouble (fromJust w) (fromJust h)
        getZone cons name = map (cons . toRect) $ getObjs name
parseMeta _ = (V2 0 0, [])


parseHooks :: Maybe Layer -> [Hook]
parseHooks layers =
  case layers of
    Just layer ->
      fmap (Hook . centerOnSquare targetRadius . getPosOfObj)
           $ layerObjects layer
    Nothing -> []


parseObjects :: Maybe Layer -> M.Map Int T.Object
parseObjects layers =
  case layers of
    Just layer -> M.fromList
                . fmap makeObject
                . zip [0..]
                $ layerObjects layer
    Nothing -> M.empty
  where

    makeObject (idx, obj@Object {..}) =
      ( idx
      , (objectMap M.! fromJust objectType)
          (objects . at idx)
          (getPosOfObj obj)
          objectProperties
      )


parseCollision :: Color -> Maybe Layer -> [Piece]
parseCollision c layers =
    case layers of
      Just ObjectLayer {layerObjects = objs} -> concatMap toPiece objs
      _ -> error "bad collision"
  where
      toPiece (Object
                { objectX = x'
                , objectY = y'
                , objectWidth = w'
                , objectHeight = h'
                , objectPolyline = l
                , objectName = maybe "" id -> name
                })
          | isJust l =
              let (Polyline pl) = fromJust l
                  ls = zip pl $ tail pl
                  x = importScale * x'
                  y = importScale * y'
                  fromPair ((ax,ay),(bx,by))
                      | ax == bx || ay == by =
                          Wall (lineBetween
                               (V2 (x + ax * importScale) (y + ay * importScale))
                               (V2 (x + bx * importScale) (y + by * importScale)))
                               c
                               name
                      | otherwise = error "level contains slopes"
               in map fromPair ls

          | otherwise =
              let x = importScale * x'
                  y = importScale * y'
                  w = importScale * fromJust w'
                  h = importScale * fromJust h'
               in [ Wall (lineRel (V2 x y)       (V2 w 0)) c name
                  , Wall (lineRel (V2 x (y + h)) (V2 w 0)) c name
                  , Wall (lineRel (V2 x y)       (V2 0 h)) c name
                  , Wall (lineRel (V2 (x + w) y) (V2 0 h)) c name
                  ]


updateLevel :: Time -> GameState -> (Level, GameState -> GameState)
updateLevel dt gs =
  let l     = _currentLevel gs
      x     = fmap (updateObject dt gs) $ _objects l
      objs' = fmap fst x
      f     = appEndo . mconcat . fmap (Endo . snd) $ F.toList x
   in ( l & objects .~ objs'
      , f
      )

  -- l
  -- { _objects = fmap (updateObject dt l p)
  --            $ _objects l
  -- }

