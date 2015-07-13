module Math ( toPair
            , Line (Line)
            , Rect (Rect)
            , inRect
            , lineBetween
            , lineRel
            , linesIntersection
            , linesIntersect
            , module Data.Vector
            ) where

import ClassyPrelude
import Data.Vector

toPair :: Vector2 -> (Double, Double)
toPair v = (v2x v, v2y v)

newtype Line = Line (Vector2, Vector2) deriving (Show, Eq)

lineBetween :: Vector2 -> Vector2 -> Line
lineBetween a b = Line (a, b - a)

lineRel :: Vector2 -> Vector2 -> Line
lineRel a rel = Line(a, rel)

-- from https://hackage.haskell.org/package/SG-1.0/docs/src/Data-SG-Geometry-TwoDim.html#intersectLines2
lineIntersect' :: Line -> Line -> Maybe (Double, Double)
lineIntersect' (Line ((Vector2 x y),(Vector2 xd yd))) (Line ((Vector2 x' y'),(Vector2 xd' yd')))
  | a == 0 = Nothing
  | otherwise = Just $ (t, t')
  where
    a = (xd' * yd) - (xd * yd')
    t' = ((xd * (y' - y)) - (yd * (x' - x))) / a
    t = ((xd' * (y - y')) - (yd' * (x - x'))) / (negate a)

linesIntersection :: Line -> Line -> Maybe Vector2
linesIntersection a@(Line (start, rel)) b = onLine $ lineIntersect' a b
  where onLine Nothing       = Nothing
        onLine (Just (x, y)) = if x >= 0 && x <= 1 && y >= 0 && y <= 1
                                  then Just $ x |* rel + start
                                  else Nothing

linesIntersect :: Line -> Line -> Bool
linesIntersect a b = onLine $ lineIntersect' a b
  where onLine Nothing       = False
        onLine (Just (x, y)) = x >= 0 && x <= 1 && y >= 0 && y <= 1

data Rect = Rect Vector2 Vector2 deriving (Show, Eq)
inRect :: Rect -> Vector2 -> Bool
inRect (Rect (Vector2 x y)
       (Vector2 w h))
       (Vector2 px py) =  x  <= px
                       && px <  x + w
                       && y  <= py
                       && py <  y + h

