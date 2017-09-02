{-# LANGUAGE NoImplicitPrelude #-}

module Collision where

import Game.Sequoia
import Linear.Metric
import Math
import Types hiding (left, right, to)


boxGeomToRect :: BoxGeom -> V2 -> Rect
boxGeomToRect b p =
  let (tl, _, _, br) = corners b p
   in Rect tl $ br - tl

corners :: BoxGeom -> V2 -> (V2, V2, V2, V2)
corners b p =
    ( V2 left  top
    , V2 right top
    , V2 left  bottom
    , V2 right bottom
    )
  where
    x = view _x p
    y = view _y p
    left   = x - leftX b
    right  = x + rightX b
    top    = y - topY b
    bottom = y + bottomY b


data Axis = AxisX | AxisY deriving Eq


withinRadius
    :: BoxGeom
    -> V2  -- ^ Position of box
    -> Double
    -> V2  -- ^ Hook
    -> Bool
withinRadius b p r t =
  any (\c -> r >= norm (t - c)) (corners b p ^.. each)
    || any (\c -> withinBox b p $ t + c ^* r)
           [ V2 1 0
           , V2 (-1) 0
           , V2 0 1
           , V2 0 (-1)
           , V2 0 0
           ]


withinBox :: BoxGeom -> V2 -> V2 -> Bool
withinBox b p t = inRect (boxGeomToRect b p) t


sweep :: BoxGeom -> V2 -> (a -> Line) -> [a] -> Axis -> Double -> (Maybe a, V2)
sweep bg pos f ls ax dx
    | ax == AxisX && dx <  0 = sweep' tl bl $ V2 1 0
    | ax == AxisX && dx >= 0 = sweep' tr br $ V2 1 0
    | ax == AxisY && dx <  0 = sweep' tl tr $ V2 0 1
    | ax == AxisY && dx >= 0 = sweep' bl br $ V2 0 1
    | otherwise              = error "bad sweep is this possible"
 where
   (tl, tr, bl, br) = corners bg pos
   sweep' c1 c2 u =
     let to = dx *^ u
         diff res = (res - c1) * u + (negate $ normalize to)
      in case listToMaybe $ mapMaybe (collision' f ls . flip lineRel to) [c1, c2] of
           Just (l, v) -> (Just l, pos + diff v)
           Nothing     -> (Nothing, pos + to)



collision' :: (a -> Line) -> [a] -> Line -> Maybe (a, V2)
collision' f ls dp = listToMaybe
                   . mapMaybe (\a -> (a,) <$> linesIntersection dp (f a))
                   $ ls
