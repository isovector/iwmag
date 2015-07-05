module Level where

type Position = (Int, Int)
type Collision = Position -> Bool
type Tiles = [[Int]]

data Level = Level { tiles :: Tiles, collision :: [Collision] }
