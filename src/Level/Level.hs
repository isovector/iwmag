{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Level.Level
  ( Piece (..)
  , Hook (..)
  , Level (..)
  , Door (..)
  , levels
  , updateLevel
  ) where

import           BasePrelude
import qualified Data.Map as M
import           Data.Tiled
import           Game.Sequoia
import           Game.Sequoia.Color
import           Linear.Vector
import           Math
import           ObjectMap
import           Actor.Constants
import qualified Types as T
import           Types hiding (Object (..))
import Object

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

drawLine :: Color -> Line -> Form
drawLine c (Line pos size) = move pos
                           . traced (solid c)
                           . segment (0,0)
                           $ unpackV2 size

levels :: [(String, Level)]
levels = zip names
       $ map (parseLayers . mapLayers)
       . unsafePerformIO
       . sequence
       . map (\n -> loadMapFile $ "level/" ++ n ++ ".tmx")
       $ names
  where names = [ "test1"
                , "test2"
                , "test3"
                ]
{-# NOINLINE levels #-}

parseLayers :: [Layer] -> Level
parseLayers ls = let dz =  map (getRect) $ getZones isDeath
                     nbz = map (getRect) $ getZones isNoBoost
                     doors = getZones isDoor
                  in col { playerSpawn  = spawn
                         , deathZones   = dz
                         , noBoostZones = nbz
                         , targets      = levelHooks
                         , _objects     = levelObjects
                         , doors = mapMaybe getDoor doors
                         , forms  = forms col
                                 ++ fmap targetForm levelHooks
                                 ++ zonesToForm dz  red
                                 ++ zonesToForm nbz cyan
                                 ++ zonesToForm (map getRect doors) purple
                         }
  where (spawn, zones) = parseMeta $ getLayer "meta"
        col            = buildLevel . parseCollision $ getLayer "collision"
        levelHooks   = parseHooks $ getLayer "targets"
        levelObjects = parseObjects $ getLayer "objects"
        getLayer name  = listToMaybe $ filter ((== name) . layerName) ls
        getZones f = filter f zones
        zonesToForm zs c = map (mkForm c) zs
        targetForm (Hook pos) = move pos
                                . outlined (dashed red)
                                $ circle targetRadius
        mkForm c (Rect pos size) = move (pos + size ^* 0.5)
                                 . outlined (solid c)
                                 . uncurry rect
                                 $ unpackV2 size


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


parseObjects :: Maybe Layer -> [T.Object]
parseObjects layers =
  case layers of
    Just layer -> fmap makeObject $ layerObjects layer
    Nothing -> []
  where
    makeObject obj@Object {..} = (objectMap M.! fromJust objectType) (getPosOfObj obj) objectProperties


parseCollision :: Maybe Layer -> [Piece]
parseCollision layers =
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
                               green
                      | otherwise = error "level contains slopes"
               in map fromPair ls

          | otherwise =
              let x = importScale * x'
                  y = importScale * y'
                  w = importScale * fromJust w'
                  h = importScale * fromJust h'
               in [ Wall (lineRel (V2 x y)       (V2 w 0)) green
                  , Wall (lineRel (V2 x (y + h)) (V2 w 0)) green
                  , Wall (lineRel (V2 x y)       (V2 0 h)) green
                  , Wall (lineRel (V2 (x + w) y) (V2 0 h)) green
                  ]

buildLevel :: [Piece] -> Level
buildLevel ((Wall l c):pxs) =
    let built = buildLevel pxs
        form  = drawLine c l
     in built { geometry = l    : (geometry built)
              , forms    = form : (forms built)
              }
buildLevel [] = Level [] [] (V2 1 0) [] [] [] [] []


updateLevel :: Time -> Actor -> Level -> Level
updateLevel dt p l = l
  { _objects = fmap (updateObject dt l p)
             $ _objects l
  }

