module Level.Level ( Piece (Wall)
                   , defaultLevel
                   , geometry
                   , forms
                   , playerSpawn
                   , deathZones
                   , Level
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

data Level = Level { geometry    :: [Line]
                   , forms       :: [Form]
                   , playerSpawn :: Vector2
                   , deathZones  :: [Rect]
                   } deriving Show

drawLine :: Color -> Line -> Form
drawLine c (Line (pos, size)) = move (toPair pos)
                              $ traced (solid c)
                              $ segment (0,0)
                              $ toPair size

defaultLevel :: Level
defaultLevel = parseLayers
             . mapLayers
             . unsafePerformIO
             $ loadMapFile "level/test1.tmx"
{-# NOINLINE defaultLevel #-}

importScale :: Double
importScale = 2

parseLayers :: [Layer] -> Level
parseLayers ls = col { playerSpawn = spawn
                     , deathZones = zones
                     }
  where (spawn, zones) = parseObjects   $ getLayer "objects"
        col            = buildLevel . parseCollision $ getLayer "collision"
        getLayer name  = headMay $ filter ((== name) . layerName) ls


getPosOfObj :: Object -> Vector2
getPosOfObj Object{objectX = x, objectY = y} =
    Vector2 (fromIntegral x * importScale) (fromIntegral y * importScale)

scaleInts :: Int -> Int -> Vector2
scaleInts x y = Vector2 (importScale * fromIntegral x) (importScale * fromIntegral y)

parseObjects :: Maybe Layer -> (Vector2, [Rect])
parseObjects (Just ObjectLayer{layerObjects = objs}) = (spawn, zones)
  where spawn = maybe (Vector2 0 0) getPosOfObj . headMay $ getObjs "spawn"
        zones = map toRect $ getObjs "death"
        getObjs t = filter (maybe False (== t) . objectType) objs
        toRect obj@(Object{objectWidth = w, objectHeight = h}) =
            Rect (getPosOfObj obj) $ scaleInts (fromJust w) (fromJust h)
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
buildLevel []               = Level [] [] vector2X []

