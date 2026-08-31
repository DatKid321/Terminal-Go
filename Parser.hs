module Parser where

import Control.Applicative (Alternative (..))
import Data.Char (isAlpha, isSpace, ord)
import Data.List (uncons, find, elemIndex)
import Data.Maybe (maybeToList, fromJust)
import Control.Monad.Trans.State (StateT(..), runStateT)
import Data.Functor (($>))

import Types

type Parser a = StateT String Maybe a

parse :: Parser a -> String -> Maybe (a, String)
parse = runStateT

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = StateT $ find (p . fst) . maybeToList . uncons

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

parseMove :: Parser (Player, Point)
parseMove = (,) <$> (charTok ';' *> parseColour) <*> parseCoords

parseGame :: Parser [(Player, Point)]
parseGame = many parseMove