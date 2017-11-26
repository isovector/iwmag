{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}

module ObjectParser where

import Actor (drawActor)
import Actor.Constants (importScale)
import Apecs.Types (Store, Storage)
import Apecs.Util (nextEntity)
import Data.Tiled.Types
import Prologue


data SomeComponent where
  SomeComponent
      :: ( Store (Storage c), Has World c
         )
      => c
      -> SomeComponent


buildObject :: Object -> Sys (Entity e)
buildObject obj = do
  let pos  = getObjectPos obj
      geom = getObjectGeom obj
      gfx  = getObjectGfx obj
      grav = getObjectGravity obj
      vel  = getObjectVel obj
      cs = pos ++ geom ++ gfx ++ grav ++ vel
  newSomeEntity cs


newSomeEntity :: [SomeComponent] -> Sys (Entity e)
newSomeEntity cs = do
  e <- nextEntity
  for_ cs ((\(SomeComponent c) -> set e c) :: SomeComponent -> Sys ())
  pure $ cast e


getObjectPos :: Object -> [SomeComponent]
getObjectPos Object{..} = pure
                        . SomeComponent
                        . Pos
                        $ V2 objectX objectY ^* importScale


getObjectGeomImpl :: Object -> Maybe Collision
getObjectGeomImpl Object{..} = do
  let withGeom = lookup "geom" objectProperties
  guard $ isNothing withGeom || withGeom == Just "true"

  w <- objectWidth
  h <- objectHeight
  pure . Collision $ BoxGeom 0 (w * importScale) 0 (h * importScale)


getObjectGeom :: Object -> [SomeComponent]
getObjectGeom = fmap SomeComponent
              . maybeToList
              . getObjectGeomImpl


getObjectGfx :: Object -> [SomeComponent]
getObjectGfx obj@Object{..} = maybeToList $ do
  let color = parseColor
            $ lookup "color" objectProperties
  Collision c <- getObjectGeomImpl obj
  pure . SomeComponent
       . Gfx
       . group
       $ drawActor color c


parseColor :: Maybe String -> Color
parseColor Nothing = rgb 1 0 1
parseColor (Just (fmap read . words -> [r,g,b]))   = rgb  r g b
parseColor (Just (fmap read . words -> [r,g,b,a])) = rgba r g b a
parseColor (Just bad) = error $ "failed to parse color: " ++ bad


getObjectGravity :: Object -> [SomeComponent]
getObjectGravity Object{..} = do
  g <- maybeToList $ lookup "gravity" objectProperties
  if g == "true"
     then [ SomeComponent Gravity
          , SomeComponent . Vel $ V2 0 0
          ]
     else []


getObjectVel :: Object -> [SomeComponent]
getObjectVel Object{..} = maybeToList $ do
  v <- lookup "vel" objectProperties
  pure . SomeComponent
       . Vel
       $ parseV2 v

parseV2 :: String -> V2
parseV2 (fmap read . words -> [x, y]) = V2 x y
parseV2 bad = error $ "failed to parse v2: " ++ bad

