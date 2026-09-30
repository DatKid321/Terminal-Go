module Board where

import Control.Applicative (liftA2)

import Types

hoshi :: Int -> [Point] -- Gets star points for a board size
hoshi size = liftA2 (,) edge edge ++ [(mid, mid) | odd size]
  where
    mid :: Int
    mid = size `div` 2 + 1

    corners :: [Int]
    corners
        | size > 11 = [4, size - 3]
        | size >= 8 = [3, size - 2]
        | otherwise = []

    sides :: [Int]
    sides = [mid | odd size, size >= 17]

    edge :: [Int]
    edge = corners ++ sides