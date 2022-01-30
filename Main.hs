module Main where

import Lisp
import Parser

import Control.Exception
import System.IO

readAndFlush :: String -> IO String
readAndFlush prompt = do
  putStr prompt
  hFlush stdout
  getLine

read_ :: IO (String)
read_ = readAndFlush "λ >> "

eval_ :: String -> Either String Expr
eval_ input =
  case parse lispExpr input of
    (Just (parsed, "")) -> Right $ eval parsed (Atom Nil)
    _                   -> Left "Unable to parse"

print_ :: Either String Expr -> IO ()
print_ result = putStrLn $
  case result of
    Right expr -> show expr
    Left err   -> err

repl :: IO ()
repl = read_ >>= (print_ . eval_) >> loop
  where loop = repl

main :: IO ()
main = repl
