{-# LANGUAGE NoImplicitPrelude #-}

module Utils ( showTrace
             , listDiff
             ) where

import BasePrelude

showTrace :: (Show a) => a -> a
showTrace = trace =<< show

-- assume these are sorted
listDiff :: (Ord a) => [a] -> [a] -> ([a], [a])
listDiff = listDiff' ([], [])
  where
      listDiff' a [] [] = a
      listDiff' (ax,ay) [] y = (ax, ay ++ y)
      listDiff' (ax,ay) x [] = (ax ++ x, ay)
      listDiff' a@(ax,ay) x@(xc:xs) y@(yc:ys)
        | xc == yc  = listDiff' a xs ys
        | xc <  yc  = listDiff' (xc:ax,ay) xs y
        | otherwise = listDiff' (ax,yc:ay) x ys

