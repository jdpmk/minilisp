module Main where

import Lisp
import Parser

import Control.Exception
import System.IO

read_ :: IO (String)
read_ = do
  putStr "λ > "
  input <- getLine
  return input

eval_ :: String -> Either String Expr
eval_ input =
  case parse lispExpr input of
    (Just (parsed, "")) -> Right $ eval parsed (Atom Nil)
    _                   -> Left "Unable to parse"

print_ :: Either String Expr -> IO ()
print_ result = putStrLn $
  case result of
    Right expr -> show $ expr
    Left err   -> err

repl :: IO ()
repl = do
  input <- read_
  let result = eval_ input
  print_ result
  loop
    where
      loop = repl

main :: IO ()
main = repl
