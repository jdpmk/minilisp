-- Copyright (c) 2022 Joydeep Mukherjee

-- Permission is hereby granted, free of charge, to any person obtaining a copy
-- of this software and associated documentation files (the "Software"), to deal
-- in the Software without restriction, including without limitation the rights
-- to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
-- copies of the Software, and to permit persons to whom the Software is
-- furnished to do so, subject to the following conditions:

-- The above copyright notice and this permission notice shall be included in all
-- copies or substantial portions of the Software.

-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
-- IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
-- FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
-- AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
-- LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
-- OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
-- SOFTWARE.

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
