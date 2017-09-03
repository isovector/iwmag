{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-missing-fields #-}

module KnotTying where

import Types
import ObjectMap
import Level.Level
import Data.Tiled
import Actor.Constants
import GameState

theLevels :: [(String, Level)]
theLevels = zip names
          $ map (\x ->
              parseLayers theObjectMap
                          (importScale *^ V2 (fromIntegral $ mapWidth x * mapTileWidth x)
                                             (fromIntegral $ mapHeight x * mapTileHeight x))
                          (fmap getTileset . listToMaybe $ mapTilesets x)
                          (mapLayers x)
                )
          . unsafePerformIO
          . sequence
          . map (\n -> loadMapFile $ "level/" ++ n ++ ".tmx")
          $ names
  where names = [ "test1"
                , "test2"
                , "test3"
                , "test4"
                ]
{-# NOINLINE theLevels #-}

initState :: GameState
initState = resetState firstLevel
          $ GameState
          { objectMap = theObjectMap
          , levels    = theLevels
          }

