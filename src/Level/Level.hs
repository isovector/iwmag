{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Level.Level
  ( Piece (..)
  , Target (..)
  , levels
  , Level (..)
  , Door (..)
  ) where

import BasePrelude
import Data.Tiled
import Game.Sequoia
import Game.Sequoia.Color
import Linear.Vector
import Math
import Player.Constants


data Piece = Wall Line Color

data Target = Target { targetPos :: V2 }
  deriving (Eq, Show)

data Level = Level { geometry     :: [Line]
                   , forms        :: [Form]
                   , playerSpawn  :: V2
                   , deathZones   :: [Rect]
                   , noBoostZones :: [Rect]
                   , doors        :: [Door]
                   , targets      :: [Target]
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

importScale :: Double
importScale = 2

parseLayers :: [Layer] -> Level
parseLayers ls = let dz =  map (getRect) $ getZones isDeath
                     nbz = map (getRect) $ getZones isNoBoost
                     doors = getZones isDoor
                  in col { playerSpawn  = spawn
                         , deathZones   = dz
                         , noBoostZones = nbz
                         , targets      = levelTargets
                         , doors = mapMaybe getDoor doors
                         , forms  = forms col
                                 ++ fmap targetForm levelTargets
                                 ++ zonesToForm dz  red
                                 ++ zonesToForm nbz cyan
                                 ++ zonesToForm (map getRect doors) purple
                         }
  where (spawn, zones) = parseObjects $ getLayer "objects"
        col            = buildLevel . parseCollision $ getLayer "collision"
        levelTargets   = parseTargets . fromJust $ getLayer "targets"
        getLayer name  = listToMaybe $ filter ((== name) . layerName) ls
        getZones f = filter f zones
        zonesToForm zs c = map (mkForm c) zs
        targetForm (Target pos) = move pos
                                . outlined (dashed red)
                                $ circle targetRadius
        mkForm c (Rect pos size) = move (pos + size ^* 0.5)
                                 . outlined (solid c)
                                 . uncurry rect
                                 $ unpackV2 size


getPosOfObj :: Object -> V2
getPosOfObj Object{objectX = x, objectY = y} =
    V2 (fromIntegral x * importScale) (fromIntegral y * importScale)

scaleInts :: Int -> Int -> V2
scaleInts x y = V2 (importScale * fromIntegral x) (importScale * fromIntegral y)

parseObjects :: Maybe Layer -> (V2, [Zone])
parseObjects (Just ObjectLayer{layerObjects = objs}) =
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
            Rect (getPosOfObj obj) $ scaleInts (fromJust w) (fromJust h)
        getZone cons name = map (cons . toRect) $ getObjs name
parseObjects _ = (V2 0 0, [])

parseTargets :: Layer -> [Target]
parseTargets = fmap (Target . getPos . getPosOfObj)
             . layerObjects
  where
    getPos pos = pos + V2 1 1 ^* (targetRadius * importScale)


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
                  x = importScale * fromIntegral x'
                  y = importScale * fromIntegral y'
                  fromPair ((ax,ay),(bx,by))
                      | ax == bx || ay == by =
                          Wall (lineBetween
                               (V2 (x + fromIntegral ax * importScale) (y + fromIntegral ay * importScale))
                               (V2 (x + fromIntegral bx * importScale) (y + fromIntegral by * importScale)))
                               green
                      | otherwise = error "level contains slopes"
               in map fromPair ls

          | otherwise =
              let x = importScale * fromIntegral x'
                  y = importScale * fromIntegral y'
                  w = importScale * (fromIntegral $ fromJust w')
                  h = importScale * (fromIntegral $ fromJust h')
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
buildLevel []               = Level [] [] (V2 1 0) [] [] [] []

