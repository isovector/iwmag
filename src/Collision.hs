module Collision where

import ClassyPrelude
import Data.Maybe (fromJust)
import Math
import Utils

data BoxGeom =
    BoxGeom { leftX   :: Double
            , rightX  :: Double
            , topY    :: Double
            , bottomY :: Double
            }

corners :: BoxGeom -> Vector2 -> (Vector2, Vector2, Vector2, Vector2)
corners b p =
    ( Vector2 left  top
    , Vector2 right top
    , Vector2 left  bottom
    , Vector2 right bottom
    )
  where x = v2x p
        y = v2y p
        left   = x - leftX b
        right  = x + rightX b
        top    = y - topY b
        bottom = y + bottomY b


data Axis = AxisX | AxisY deriving Eq

sweep :: BoxGeom -> Vector2 -> [Line] -> Axis -> Double -> (Maybe Line, Vector2)
sweep bg pos ls ax dx
    | ax == AxisX && dx <  0 = sweep' tl bl vector2X
    | ax == AxisX && dx >= 0 = sweep' tr br vector2X
    | ax == AxisY && dx <  0 = sweep' tl tr vector2Y
    | ax == AxisY && dx >= 0 = sweep' bl br vector2Y
 where (tl, tr, bl, br) = corners bg pos
       swap (Vector2 x y) = Vector2 y x
       sweep' c1 c2 u =
           let to = dx |* u
               diff res = (res - c1) * u + (negate $ vnormalise to)
               tests    = mapMaybe (collision' ls . flip lineRel to) [c1, c2]
               (Just (l, v)) = headMay tests
               occluded  = (Just l, pos + diff v)
               s = fromJust $ slope l
               rise = swap u *| if ax == AxisX
                                   then dx * s
                                   else dx / s

            in case length tests of
                 2 -> occluded
                 1 -> let (Just (l, v)) = headMay tests
                       in if isNothing $ slope l
                              then occluded
                              else (Just l, pos + diff v + rise)
                 0 -> (Nothing, pos + to)



collision' :: [Line] -> Line -> Maybe (Line, Vector2)
collision' ls dp = headMay
                 . map (\a -> (a, fromJust $ linesIntersection dp a))
                 . filter (isJust . linesIntersection dp)
                 $ ls
