{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE NoImplicitPrelude #-}

module ObjectParser where

import Actor (drawActor)
import Actor.Constants (importScale)
import Data.Tiled.Types
import Prologue



buildObject :: Object -> Sys Entity
buildObject obj =
  newSomeEntity $ mconcat
    [ getObjectPos     obj
    , getObjectGeom    obj
    , getObjectGfx     obj
    , getObjectGravity obj
    , getObjectVel     obj
    , getObjectSwoop   obj
    , getHeldBy        obj
    ]


newSomeEntity :: Endo Entity -> Sys Entity
newSomeEntity f = do
  e <- newEntity $ appEndo f defEntity
  getEntity e


getObjectPos :: Object -> Endo Entity
getObjectPos Object{..} = Endo $ \ent -> ent
  { pos = Just $ V2 objectX objectY ^* importScale
  }


getObjectGeomImpl :: Object -> Maybe BoxGeom
getObjectGeomImpl Object{..} = do
  w <- (*importScale) <$> objectWidth
  h <- (*importScale) <$> objectHeight
  pure $ BoxGeom (w / 2) (w / 2) (h / 2) (h / 2)


maybeToEndo :: Maybe (a -> a) -> Endo a
maybeToEndo (Just a) = Endo a
maybeToEndo Nothing = Endo id

getObjectGeom :: Object -> Endo Entity
getObjectGeom obj@Object{objectProperties} = maybeToEndo $ do
  let withGeom = lookup "geom" objectProperties
  guard $ isNothing withGeom || withGeom == Just "true"

  pure $ \ent -> ent { collision = getObjectGeomImpl obj }


getObjectGfx :: Object -> Endo Entity
getObjectGfx obj@Object{..} = maybeToEndo $ do
  let color = parseColor
            $ lookup "color" objectProperties
  c <- getObjectGeomImpl obj
  pure $ \ent -> ent
    { gfx = Just . group $ drawActor color c
    }


parseColor :: Maybe String -> Color
parseColor Nothing = rgb 1 0 1
parseColor (Just (fmap read . words -> [r,g,b]))   = rgb  r g b
parseColor (Just (fmap read . words -> [r,g,b,a])) = rgba r g b a
parseColor (Just bad) = error $ "failed to parse color: " ++ bad


getObjectGravity :: Object -> Endo Entity
getObjectGravity Object{..} = maybeToEndo $ do
  g <- lookup "gravity" objectProperties
  guard $ g == "true"
  pure $ \ent -> ent
    { gravity = Just ()
    , vel = Just $ V2 0 0
    }

getHeldBy :: Object -> Endo Entity
getHeldBy Object{..} = maybeToEndo $ do
  g <- lookup "held" objectProperties
  guard $ g == "true"
  pure $ \ent -> ent
    { heldBy = Just $ Ent 0
    }


getObjectSwoop :: Object -> Endo Entity
getObjectSwoop Object{..} = maybeToEndo $ do
  offset <- parseV2 <$> lookup "offset" objectProperties
  let duration = 8
      phase  = maybe duration read $ lookup "phase" objectProperties
  pure $ \ent -> ent
    { swoop = Just $ Swoop offset SwoopHover phase duration
    , vel   = Just $ V2 0 0
    , termVel = Just 250
    }


getObjectVel :: Object -> Endo Entity
getObjectVel Object{..} = maybeToEndo $ do
  v <- lookup "vel" objectProperties
  pure $ \ent -> ent
    { vel = Just $ parseV2 v
    }

parseV2 :: String -> V2
parseV2 (fmap read . words -> [x, y]) = V2 x y
parseV2 bad = error $ "failed to parse v2: " ++ bad

