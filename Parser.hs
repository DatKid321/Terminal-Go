module Parser where

import Control.Applicative (Alternative (..))
import Data.Char (isSpace)
import Data.List (uncons, find, elemIndex)
import Data.Maybe (maybeToList, fromJust)
import Control.Monad (mfilter)
import Control.Monad.Trans.State (StateT(..), runStateT)
import Data.Functor (($>))

import Types

type Parser a = StateT String Maybe a

parse :: Parser a -> String -> Maybe (a, String)
parse = runStateT

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = StateT $ mfilter (p . fst) . uncons

is :: Char -> Parser Char
is = satisfy . (==)

space :: Parser Char
space = satisfy isSpace

letters :: String
letters = ['a' .. 'z'] ++ ['A' .. 'Z']

alpha :: Parser Char
alpha = satisfy (`elem` letters)

spaces :: Parser String
spaces = many space

tok :: Parser a -> Parser a
tok = (<* spaces)

charTok :: Char -> Parser Char
charTok = tok . is

parseColour :: Parser Player
parseColour = (is 'B' $> Black) <|> (is 'W' $> White)

parseCoords :: Parser Point
parseCoords = (,) <$> (charTok '[' *> alphaIndex) <*> (alphaIndex <* charTok ']')
  where
    alphaIndex :: Parser Int
    alphaIndex = (+1) . fromJust . (`elemIndex` letters) <$> alpha

parseTurn :: Parser Turn
parseTurn = (charTok '[' *> charTok ']' $> Pass) <|> (Move <$> parseCoords)

parseMove :: Parser (Player, Turn)
parseMove = (,) <$> (charTok ';' *> parseColour) <*> parseTurn

parseGame :: Parser [(Player, Turn)]
parseGame = charTok '(' *> many parseMove <* charTok ')'