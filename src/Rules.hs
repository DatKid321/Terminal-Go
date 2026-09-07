module Rules where

-- If there are no lists, then
-- import Prelude hiding (filter, map)

import Data.Array (listArray, (!), (//))
import Data.Bool (bool)
import Data.Ix (inRange, range)
import Data.Function (on)
import Data.Maybe (isNothing)
import Control.Applicative (liftA2)

-- Import set specific functions
import Data.Set (Set)
import qualified Data.Set as Set

import Types

-- Good for partial application:
-- Functions taking only rules first are Geometric, position is not needed
-- Functions taking rules then position are Queries, fix rules and state before asking
-- Functions taking position to position are Transformative, position to position is natural

bounds :: Rules -> (Point, Point)
bounds rules = ((1, 1), (size rules, size rules))

blank :: Rules -> Position
blank rules = Position $ listArray (bounds rules) $ repeat Nothing

points :: Rules -> [Point]
points = range . bounds

inside :: Rules -> Point -> Bool
inside = inRange . bounds

colour :: Position -> Point -> Colour
colour = (!) . colours

setPoint :: Point -> Colour -> Position -> Position
setPoint point mark pos = Position $ colours pos // [(point, mark)]

vacant :: Position -> Point -> Bool
vacant pos = isNothing . colour pos

neighbours :: Rules -> Point -> Set Point -- Get neighbours of a point
neighbours rules (x, y) = Set.filter (inside rules) [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]

string :: Rules -> Position -> Point -> Group -- Get connected region containing point
string rules pos point = Set.unions $ expand [point] []
  where
    same :: Point -> Bool -- Check if point has same colour
    same = on (==) (colour pos) point

    expand :: Set Point -> Set Point -> [Set Point] -- Expand region by a layer
    expand curr prev = curr : bool (expand next curr) [] (Set.null next)
      where
        next :: Set Point -- Next unvisited layer
        next = Set.filter same $ Set.difference (foldMap (neighbours rules) curr) prev

liberties :: Rules -> Position -> Group -> Set Point -- Get empty points adjacent to a group
liberties rules pos = Set.filter (vacant pos) . foldMap (neighbours rules)

clear :: Rules -> Set Point -> Position -> Position -- Remove groups with no liberties
clear rules targets pos = Position $ colours pos // map (, Nothing) (Set.toList captured)
  where
    groups :: Set Group -- Groups containing given points
    groups = Set.map (string rules pos) targets

    captured :: Set Point -- Points in groups with no liberties
    captured = Set.unions $ Set.filter (Set.null . liberties rules pos) groups

move :: Rules -> Player -> Point -> Position -> Position -- Place a stone
move rules player point pos = clear rules [point] cleared
  where
    placed :: Position -- Position after placing stone
    placed = setPoint point (Just player) pos

    enemy :: Point -> Bool -- Check point contains an enemy stone
    enemy = maybe False (/= player) . colour placed

    opponents :: Set Point -- Adjacent enemy stones
    opponents = Set.filter enemy $ neighbours rules point

    cleared :: Position -- Position after removing enemy groups
    cleared = clear rules opponents placed

play :: Rules -> Player -> Turn -> History -> Either Illegal History -- Place a stone if legal
play _     _      Pass         past@(pos : _) = Right $ pos : past   -- Record pass
play rules player (Move point) past@(pos : _)
    | not $ inside rules point                = Left Outside         -- Point is off board
    | not $ vacant pos point                  = Left Occupied        -- Point contains stone
    | next `elem` past                        = Left Superko         -- Position has been repeated
    | otherwise                               = Right $ next : past  -- Position is legal
  where
    next :: Position -- Position after move
    next = move rules player point pos

{-
ended :: History -> Bool
ended (pos : prev : before : _) = pos == prev && prev = before
ended _                         = False
-}

ended :: History -> Bool
ended = Set.null . Set.deleteMin . Set.fromList . take 3 -- May fail pass on turn 1

score :: Rules -> Position -> Player -> Int
score rules pos player = length $ filter owned $ points rules
  where
    owned :: Point -> Bool
    owned point = maybe (borders point) Set.singleton (colour pos point) == [player]

    borders :: Point -> Set Player
    borders = foldMap (foldMap Set.singleton . colour pos) . foldMap (neighbours rules) . string rules pos