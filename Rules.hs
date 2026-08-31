module Rules where

import Data.List (nub, (\\))
import Data.Bool

import Types

inside :: Int -> Point -> Bool -- Checks if a point is inside a board
inside size (x, y) = all (liftA2 (&&) (1 <=) (<= size)) [x, y]

neighbours :: Int -> (Int, Int) -> [Point] -- Get neighbours of a point
neighbours size (x, y) = filter (inside size) [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]

string :: Int -> Board -> Point -> [Point] -- Get connected stones via flood fill
string size board point = concat $ expand [point] []
  where
    same :: Point -> Bool
    same cand = board point == board cand

    expand :: [Point] -> [Point] -> [[Point]]
    expand curr prev = curr : bool (expand next curr) [] (null next)
      where
        next :: [Point]
        next = nub (filter same . neighbours size =<< curr) \\ prev