module Parser where

import Control.Applicative (Alternative (..))

newtype Parser a = Parser { parse :: String -> Maybe (a, String) }

instance Functor Parser where
  fmap f (Parser p) = Parser $ \input -> do
    (parsed, rest) <- p input
    pure (f parsed, rest)

instance Applicative Parser where
  pure v = Parser $ \input -> Just (v, input)

  Parser f <*> Parser p = Parser $ \input -> do
    (f', rest)      <- f input
    (parsed, rest') <- p input
    pure (f' parsed, rest')

instance Monad Parser where
  Parser p >>= f = Parser $ \input -> do
    (parsed, rest) <- p input
    parse (f parsed) rest

instance Alternative Parser where
  empty = Parser $ \input -> Nothing

  Parser l <|> Parser r = Parser $ \input ->
    case l input of
      Nothing ->
        case r input of
          Nothing -> Nothing
          Just (parsed, rest) -> Just (parsed, rest)
      Just (parsed, rest) -> Just (parsed, rest)

char :: Char -> Parser Char
char c = Parser $ \input ->
  case input of
    (c':cs) -> if c == c' then Just (c, cs) else Nothing
    ""      -> Nothing

string :: String -> Parser String
string ""       = Parser $ \input -> Just ("", input)
string s@(c:cs) = Parser $ \input -> do
  (_, rest)  <- parse (char c) input
  (_, rest') <- parse (string cs) rest
  pure (s, rest')
