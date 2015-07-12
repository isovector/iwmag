module Level.Level ( Piece (Rect, Floor)
                   , defaultLevel
                   , geomWalls
                   , geomFloor
                   , forms
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
parseLayers ls =
    case headMay ls of
      Just ObjectLayer
        { layerObjects = objs
        } -> buildLevel $ concatMap toPiece objs
  where
      toPiece (Object
                { objectX = x'
                , objectY = y'
                , objectWidth = w'
                , objectHeight = h'
                }) = let x = fromIntegral x' * 2
                         y = fromIntegral y' * 2
                         w = fromIntegral $ fromJust w' * 2
                         h = fromIntegral $ fromJust h' * 2
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

