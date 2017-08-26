{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Math ( unpackV2
            , Line (Line)
            , Rect (Rect)
            , inRect
            , lineBetween
            , lineRel
            , linesIntersection
            , linesIntersect
            ) where

import BasePrelude
import Game.Sequoia
import Linear.Vector

data Line = Line V2 V2 deriving (Show, Eq)

lineBetween :: V2 -> V2 -> Line
lineBetween a b = Line a $ b - a

lineRel :: V2 -> V2 -> Line
lineRel a rel = Line a rel

-- from https://hackage.haskell.org/package/SG-1.0/docs/src/Data-SG-Geometry-TwoDim.html#intersectLines2
lineIntersect' :: Line -> Line -> Maybe (Double, Double)
lineIntersect' (Line (V2 x y)   (V2 xd yd))
               (Line (V2 x' y') (V2 xd' yd'))
  | a == 0 = Nothing
  | otherwise = Just $ (t, t')
  where
    a = (xd' * yd) - (xd * yd')
    t' = ((xd * (y' - y)) - (yd * (x' - x))) / a
    t = ((xd' * (y - y')) - (yd' * (x - x'))) / (negate a)

linesIntersection :: Line -> Line -> Maybe V2
linesIntersection a@(Line start rel) b = onLine $ lineIntersect' a b
  where onLine Nothing       = Nothing
        onLine (Just (x, y)) = if x >= 0 && x <= 1 && y >= 0 && y <= 1
                                  then Just $ x *^ rel + start
                                  else Nothing

linesIntersect :: Line -> Line -> Bool
linesIntersect a b = onLine $ lineIntersect' a b
  where onLine Nothing       = False
        onLine (Just (x, y)) = x >= 0 && x <= 1 && y >= 0 && y <= 1

data Rect = Rect V2 V2 deriving (Show, Eq)
inRect :: Rect -> V2 -> Bool
inRect (Rect (V2 x y) (V2 w h))
       (V2 px py) =  x  <= px
                  && px <  x + w
                  && y  <= py
                  && py <  y + h

