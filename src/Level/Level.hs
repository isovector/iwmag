module Level.Level ( Piece (Wall)
                   , levels
                   , geometry
                   , forms
                   , playerSpawn
                   , deathZones
                   , noBoostZones
                   , doors
                   , Level (Level)
                   , Door (Door)
                   ) where

import ClassyPrelude
import Math
import Utils
import FRP.Helm
import FRP.Helm.Color
import FRP.Helm.Signal
import Data.Tiled
import Data.Maybe (fromJust)
import System.IO.Unsafe (unsafePerformIO)

data Piece = Wall Line Color

data Level = Level { geometry     :: [Line]
                   , forms        :: [Form]
                   , playerSpawn  :: Vector2
                   , deathZones   :: [Rect]
                   , noBoostZones :: [Rect]
                   , doors        :: [Door]
                   } deriving Show

data Zone = Death   Rect
          | NoBoost Rect
          | DoorZ   Door
          deriving Show
data Door = Door Rect String deriving Show

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
drawLine c (Line (pos, size)) = move (toPair pos)
                              $ traced (solid c)
                              $ segment (0,0)
                              $ toPair size

levels :: [(String, Level)]
levels = zip names
       $ map (parseLayers . mapLayers)
       . unsafePerformIO
       . sequence
       . map (\n -> loadMapFile $ "level/" ++ n ++ ".tmx")
       $ names
  where names = [ "test1"
                , "test2"
                ]
{-# NOINLINE levels #-}

importScale :: Double
importScale = 2

parseLayers :: [Layer] -> Level
parseLayers ls = let dz =  map (getRect) $ getZones isDeath
                     nbz = map (getRect) $ getZones isNoBoost
                     doors = getZones isDoor
                  in col { playerSpawn  = spawn
                         , deathZones   = dz
                         , noBoostZones = nbz
                         , doors = mapMaybe getDoor doors
                         , forms  = forms col
                                  ++ zonesToForm dz  red
                                  ++ zonesToForm nbz cyan
                                  ++ zonesToForm (map getRect doors) purple
                         }
  where (spawn, zones) = parseObjects   $ getLayer "objects"
        col            = buildLevel . parseCollision $ getLayer "collision"
        getLayer name  = headMay $ filter ((== name) . layerName) ls
        getZones f = filter f zones
        zonesToForm zs c = map (mkForm c) zs
        mkForm c (Rect pos size) = move (toPair $ pos + size *| 0.5)
                                 . outlined (solid c)
                                 . uncurry rect
                                 $ toPair size


getPosOfObj :: Object -> Vector2
getPosOfObj Object{objectX = x, objectY = y} =
    Vector2 (fromIntegral x * importScale) (fromIntegral y * importScale)

scaleInts :: Int -> Int -> Vector2
scaleInts x y = Vector2 (importScale * fromIntegral x) (importScale * fromIntegral y)

parseObjects :: Maybe Layer -> (Vector2, [Zone])
parseObjects (Just ObjectLayer{layerObjects = objs}) =
    (spawn, deaths ++ noboosts ++ doors)
  where spawn = maybe (Vector2 0 0) getPosOfObj . headMay $ getObjs "spawn"
        deaths = getZone Death "death"
        noboosts = getZone NoBoost "noboost"
        doors = map (\d -> DoorZ
                         . Door (toRect d)
                         . fromJust
                         . lookup "to"
                         $ objectProperties d) $ getObjs "door"

        getObjs t = filter (maybe False (== t) . objectType) objs
        toRect obj@(Object{objectWidth = w, objectHeight = h}) =
            Rect (getPosOfObj obj) $ scaleInts (fromJust w) (fromJust h)
        getZone cons name = map (cons . toRect) $ getObjs name
parseObjects _ = (Vector2 0 0, [])

parseCollision :: Maybe Layer -> [Piece]
parseCollision layers =
    case headMay layers of
      Just ObjectLayer
        { layerObjects = objs
        } -> concatMap toPiece objs
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
                  x = importScale * fromIntegral x'
                  y = importScale * fromIntegral y'
                  fromPair ((ax,ay),(bx,by))
                      | ax == bx || ay == by =
                          Wall (lineBetween
                               (Vector2 (x + fromIntegral ax * importScale) (y + fromIntegral ay * importScale))
                               (Vector2 (x + fromIntegral bx * importScale) (y + fromIntegral by * importScale)))
                               green
                      | otherwise = error "level contains slopes"
               in map fromPair ls

          | otherwise =
              let x = importScale * fromIntegral x'
                  y = importScale * fromIntegral y'
                  w = importScale * (fromIntegral $ fromJust w')
                  h = importScale * (fromIntegral $ fromJust h')
               in [ Wall (lineRel (Vector2 x y)       (Vector2 w 0)) green
                  , Wall (lineRel (Vector2 x (y + h)) (Vector2 w 0)) green
                  , Wall (lineRel (Vector2 x y)       (Vector2 0 h)) green
                  , Wall (lineRel (Vector2 (x + w) y) (Vector2 0 h)) green
                  ]

buildLevel :: [Piece] -> Level
buildLevel ((Wall l c):pxs) =
    let built = buildLevel pxs
        form = drawLine c l
     in built { geometry = l    : (geometry built)
              , forms    = form : (forms built)
              }
buildLevel []               = Level [] [] vector2X [] [] []

