module Parser where

import Lisp

import Control.Applicative (Alternative (..))
import Control.Monad (guard)
import Data.Char (isAlpha, isDigit)

newtype Parser a = Parser { parse :: String -> Maybe (a, String) }

instance Functor Parser where
  fmap f (Parser p) = Parser $ \input -> do
    (parsed, rest) <- p input
    pure (f parsed, rest)

instance Applicative Parser where
  pure v = Parser $ \input -> Just (v, input)

  Parser f <*> Parser p = Parser $ \input -> do
    (f', rest)      <- f input
    (parsed, rest') <- p rest
    pure (f' parsed, rest')

instance Monad Parser where
  Parser p >>= f = Parser $ \input -> do
    (parsed, rest) <- p input
    parse (f parsed) rest

instance Alternative Parser where
  empty = Parser $ \input -> Nothing

  Parser l <|> Parser r = Parser $ \input ->
    l input <|> r input

anyChar :: Parser Char
anyChar = Parser $ \input ->
  case input of
    (c:cs) -> Just (c, cs)
    ""     -> Nothing

char :: Char -> Parser Char
char c = anyChar `satisfying` (== c)

string :: String -> Parser String
string = traverse char

satisfying :: Parser a -> (a -> Bool) -> Parser a
satisfying (Parser p) pred = Parser $ \input -> do
  (parsed, rest) <- p input
  guard $ pred parsed
  pure (parsed, rest)

failEmpty :: Parser [a] -> Parser [a]
failEmpty (Parser p) = Parser $ \input -> do
  (parsed, rest) <- p input
  if null parsed then Nothing else Just (parsed, rest)

lispIdentifier :: Parser Symbol
lispIdentifier = Identifier <$> (Parser $ \input -> do
  (parsed, rest)   <- parse (anyChar `satisfying` isAlpha) input
  (parsed', rest') <- parse (many anyChar) rest
  pure (parsed:parsed', rest'))

lispString :: Parser Symbol
lispString = String <$> (char '"' *> (many $ anyChar `satisfying` ((/=) '"')) <* char '"')

lispInteger :: Parser Symbol
lispInteger = (Integer . read) <$> failEmpty (many $ anyChar `satisfying` isDigit)

lispCar :: Parser Function
lispCar = const CAR <$> string "car"

lispCdr :: Parser Function
lispCdr = const CDR <$> string "cdr"

lispCons :: Parser Function
lispCons = const CONS <$> string "cons"

lispEqq :: Parser Function
lispEqq = const EQQ <$> string "eq"

lispAtom :: Parser Function
lispAtom = const ATOM <$> string "atom"

lispLambda :: Parser Function
lispLambda = const LAMBDA <$> string "lambda"

lispLabel :: Parser Function
lispLabel = const LABEL <$> string "label"

lispQuote :: Parser Function
lispQuote = const QUOTE <$> string "quote"

lispCond :: Parser Function
lispCond = const COND <$> string "cond"

lispFunction :: Parser Function
lispFunction = lispCar
               <|> lispCdr
               <|> lispCons
               <|> lispEqq
               <|> lispAtom
               <|> lispLambda
               <|> lispLabel
               <|> lispQuote
               <|> lispCond

lispSymbol :: Parser Symbol
lispSymbol = lispString
             <|> lispInteger
             <|> lispIdentifier
