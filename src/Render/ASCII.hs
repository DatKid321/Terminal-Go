module Render.ASCII where

import Control.Applicative (liftA2)
import Data.Bool (bool)
import Data.List (intercalate)

import Board
import Rules
import Types

data Edge
    = First
    | Middle
    | Last
    deriving (Show, Eq, Enum)

type Location = (Edge, Edge)

data Reset
    = None
    | Store
    | Full
    deriving (Show, Eq)

type RGB = (Int, Int, Int)

wrap :: String -> String -> String -> String
wrap pre suf = (pre ++) . (++ suf)

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

location :: Int -> Point -> Location
location size (x, y) = (edge y, edge x)
  where
    edge :: Int -> Edge
    edge n
        | n == 1    = First
        | n == size = Last
        | otherwise = Middle

row :: Rules -> Position -> Int -> [String]
row rules pos y = map ((point rules <*> colour pos) . (, y)) [1 .. size rules]

point :: Rules -> Point -> Colour -> String
point rules pos colour = case colour of
    Nothing     -> bool (glyph $ location (size rules) pos) "*" $ pos `elem` hoshi (size rules)
    Just player -> ansi Store Nothing (Just (255 * fromEnum player, 255 * fromEnum player, 255 * fromEnum player)) "●"
  where
    glyphs :: [String]
    glyphs = ["┌┬┐", "├┼┤", "└┴┘"]

    glyph :: Location -> String
    glyph (row, col) = [glyphs !! fromEnum row !! fromEnum col]

render :: Rules -> Position -> String
render rules pos = unlines $ map (style . intercalate "─" . row rules pos) [1 .. size rules]
  where
    style :: String -> String
    style = ansi Full (Just (242, 176, 108)) (Just (0, 0, 0)) . wrap " " " "