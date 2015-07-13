module Utils ( showTrace
             , listDiff
             , tail
             , module FRP.Helm.Sample
             ) where

import ClassyPrelude
import FRP.Helm.Signal
import FRP.Helm.Sample
import Debug.Trace

showTrace :: (Show a) => a -> a
showTrace x = trace (show x) x

-- assume these are sorted
listDiff :: (Eq a, Ord a) => [a] -> [a] -> ([a], [a])
listDiff = listDiff' ([], [])
  where
      listDiff' a [] [] = a
      listDiff' (ax,ay) [] y = (ax, ay ++ y)
      listDiff' (ax,ay) x [] = (ax ++ x, ay)
      listDiff' a@(ax,ay) x@(xc:xs) y@(yc:ys)
        | xc == yc  = listDiff' a xs ys
        | xc <  yc  = listDiff' (xc:ax,ay) xs y
        | otherwise = listDiff' (ax,yc:ay) x ys

tail :: [a] -> [a]
tail (a:as) = as

