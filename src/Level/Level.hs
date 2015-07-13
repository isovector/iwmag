module Level.Level ( Piece (Rect, Floor)
                   , defaultLevel
                   , geomWalls
                   , geomFloor
                   , forms
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

data Piece = Rect Vector2 Vector2 Color
           | Floor Line Color
           | Wall Line Color

data Level = Level { geomFloor :: [Line]
                   , geomWalls :: [Line]
                   , forms     :: [Form]
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


parseLayers :: [Layer] -> Level
parseLayers layers =
    case headMay layers of
      Just ObjectLayer
        { layerObjects = objs
        } -> buildLevel $ concatMap toPiece objs
  where
      scale = 2
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
                  x = scale * fromIntegral x'
                  y = scale * fromIntegral y'
                  fromPair ((ax,ay),(bx,by))
                      | ax == bx =
                          Wall (lineBetween
                               (Vector2 (x + fromIntegral ax * scale) (y + fromIntegral ay * scale))
                               (Vector2 (x + fromIntegral bx * scale) (y + fromIntegral by * scale)))
                               green
                      | ay == by =
                          Floor (lineBetween
                               (Vector2 (x + fromIntegral ax * scale) (y + fromIntegral ay * scale))
                               (Vector2 (x + fromIntegral bx * scale) (y + fromIntegral by * scale)))
                               green
               in map fromPair ls

          | otherwise =
              let x = scale * fromIntegral x'
                  y = scale * fromIntegral y'
                  w = scale * (fromIntegral $ fromJust w')
                  h = scale * (fromIntegral $ fromJust h')
               in [ Floor (lineRel (Vector2 x y)       (Vector2 w 0)) green
                  , Floor (lineRel (Vector2 x (y + h)) (Vector2 w 0)) green
                  , Wall  (lineRel (Vector2 x y)       (Vector2 0 h)) green
                  , Wall  (lineRel (Vector2 (x + w) y) (Vector2 0 h)) green
                  ]



buildLevel :: [Piece] -> Level
buildLevel ((Floor l c):pxs) =
    let built = buildLevel pxs
        form = drawLine c l
     in built { geomFloor = l : (geomFloor built)
              , forms     = form : (forms built)
              }
buildLevel ((Wall l c):pxs) =
    let built = buildLevel pxs
        form = drawLine c l
     in built { geomWalls = l : (geomWalls built)
              , forms     = form : (forms built)
              }
buildLevel (p:pxs)          = buildLevel pxs
buildLevel []               = Level [] [] []

