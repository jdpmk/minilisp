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

satisfying :: Parser a -> (a -> Bool) -> Parser a
satisfying (Parser p) pred = Parser $ \input -> do
  (parsed, rest) <- p input
  guard $ pred parsed
  pure (parsed, rest)

failEmpty :: Parser [a] -> Parser [a]
failEmpty (Parser p) = Parser $ \input -> do
  (parsed, rest) <- p input
  if null parsed then Nothing else Just (parsed, rest)

anyChar :: Parser Char
anyChar = Parser $ \input ->
  case input of
    (c:cs) -> Just (c, cs)
    ""     -> Nothing

char :: Char -> Parser Char
char c = anyChar `satisfying` (== c)

string :: String -> Parser String
string = traverse char

whitespace :: Parser String
whitespace = many $ anyChar `satisfying` (`elem` " \t\n")

sep :: Parser a -> Parser b -> Parser [b]
sep delimiter element = (:) <$> element <*> many (delimiter *> element) <|> pure []

lispNil :: Parser Expr
lispNil = const (Atom Nil) <$> string "nil"

lispT :: Parser Expr
lispT = const (Atom T) <$> string "t"

lispIdentifier :: Parser Expr
lispIdentifier = (Atom . Identifier) <$> identifier
  where
    identifier = Parser $ \input -> do
      (parsed, rest)   <- parse (anyChar `satisfying` isAlpha) input
      (parsed', rest') <- parse (many $ anyChar `satisfying` (`notElem` " \t\n()")) rest
      pure (parsed:parsed', rest')

lispString :: Parser Expr
lispString = (Atom . String) <$> (char '"' *> (many $ anyChar `satisfying` ((/=) '"')) <* char '"')

lispInteger :: Parser Expr
lispInteger = (Atom . Integer . read) <$> failEmpty (many $ anyChar `satisfying` isDigit)

lispKeyword :: String -> Parser Expr
lispKeyword s = Atom <$> Identifier <$> string s

lispCar    = lispKeyword "car"
lispCdr    = lispKeyword "cdr"
lispCons   = lispKeyword "cons"
lispAtom   = lispKeyword "atom"
lispEq     = lispKeyword "eq"
lispLambda = lispKeyword "lambda"
lispLabel  = lispKeyword "label"
lispQuote  = lispKeyword "quote"
             <|> (Cons (Atom (Identifier "quote"))
                   <$> (flip Cons (Atom Nil))
                   <$> (char '\'' *> lispExpr))
lispCond   = lispKeyword "cond"

lispKeywords :: Parser Expr
lispKeywords = lispCar
               <|> lispCdr
               <|> lispCons
               <|> lispAtom
               <|> lispEq
               <|> lispLambda
               <|> lispLabel
               <|> lispQuote
               <|> lispCond

lispAtomic :: Parser Expr
lispAtomic = lispNil
             <|> lispT
             <|> lispString
             <|> lispInteger
             <|> lispKeywords
             <|> lispIdentifier

lispExpr :: Parser Expr
lispExpr = lispAtomic
           <|> buildCons <$> (char '(' *> whitespace *> (sep whitespace (lispAtomic <|> lispExpr)) <* whitespace <* char ')')
  where
    buildCons []     = Atom Nil
    buildCons (x:xs) = Cons x (buildCons xs)

extract :: Maybe (Expr, String) -> Expr
extract = fst . maybe (Atom Nil, "") id

parseAndExtract :: String -> Expr
parseAndExtract = extract . parse lispExpr
