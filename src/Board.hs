module Board where

import Data.List (intercalate)
import Data.Bool (bool)
import Control.Applicative (liftA2)
import Control.Monad (ap)

import Types
import Rules

-- | Denotes the position of a point along an axis
data Edge
    = First   -- ^ On the close edge
    | Middle  -- ^ Away from eiher edge
    | Last    -- ^ On the far edge
    deriving (Show, Eq, Enum)

-- | Location of a point relative to the board edges
type Location = (Edge, Edge)

data Reset
    = None
    | Store
    | Full
    deriving (Show, Eq)

type RGB = (Int, Int, Int)

-- Helpers

wrap :: String -> String -> String -> String -- Add a prefix and suffix to a string
wrap pre suf = (pre ++) . (++ suf)

tup :: (Show a, Read t) => Int -> a -> t -- Create tuple with value repeated n times (just kinda fun)
tup n = read . wrap "(" ")" . intercalate "," . replicate n . show

ansi :: Reset -> Maybe RGB -> Maybe RGB -> String -> String -- Colours a string
ansi res bg fg str = case res of
    None  -> ansiString                              -- Keeps colour
    Store -> wrap "\ESC7" "\ESC8\ESC[1C" ansiString  -- Resets colour to previous
    Full  -> ansiString ++ "\ESC[0m"                 -- Resets colour to default
  where
    ansiColour :: Int -> Maybe RGB -> String
    ansiColour n = maybe "" $ \(r, g, b) -> "\ESC[" ++ show n ++ ";2;" ++ show r ++ ";" ++ show g ++ ";" ++ show b ++ "m"

    ansiString :: String
    ansiString = ansiColour 48 bg ++ ansiColour 38 fg ++ str

-- For creating an empty board

hoshi :: Int -> [Point] -- Gets star points for a board size
hoshi size = liftA2 (,) edge edge ++ [tup 2 mid | odd size]
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

getLoc :: Int -> Point -> Location -- Gets location of point (corner, side, centre)
getLoc size (x, y) = (loc y, loc x)
  where
    loc :: Int -> Edge
    loc n
        | n == 1    = First
        | n == size = Last
        | otherwise = Middle

-- For printing an established board

row :: Rules -> Position -> Int -> [String]
row rules pos y = map ((printPoint rules <*> colour pos) . (, y)) [1 .. size rules]

printPoint :: Rules -> Point -> Colour -> String
printPoint rules pos colour = case colour of
    Nothing     -> bool (glyph $ getLoc (size rules) pos) "*" $ pos `elem` hoshi (size rules)
    Just player -> ansi Store Nothing (Just $ tup 3 $ 255 * fromEnum player) "●"
  where
    glyphs :: [String]
    glyphs = ["┌┬┐", "├┼┤", "└┴┘"]

    glyph :: Location -> String
    glyph (row, col) = [glyphs !! fromEnum row !! fromEnum col]

printBoard :: Rules -> Position -> String
printBoard rules pos = unlines $ map (style . intercalate "─" . row rules pos) [1 .. size rules]
  where
    style :: String -> String
    style = ansi Full (Just (242, 176, 108)) (Just (0, 0, 0)) . wrap " " " "