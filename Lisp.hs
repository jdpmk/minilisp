module Lisp where

data Symbol = Identifier String
            | String String
            | Integer Int
            deriving (Show, Eq)

data Expr = Nil
          | T
          | Atom Symbol
          | Function Function
          | Cons Expr Expr
          deriving (Show, Eq)

data Function = CAR
              | CDR
              | CONS
              | EQQ
              | ATOM
              | LAMBDA
              | LABEL
              | QUOTE
              | COND
              deriving (Show, Eq)

car :: Expr -> Expr
car Nil          = Nil
car T            = error "`car` is undefined for atomic arguments"
car (Atom _)     = error "`car` is undefined for atomic arguments"
car (Function _) = error "`car` is undefined for atomic arguments"
car (Cons a _)   = a

cdr :: Expr -> Expr
cdr Nil          = Nil
cdr T            = error "`cdr` is undefined for atomic arguments"
cdr (Atom _)     = error "`cdr` is undefined for atomic arguments"
cdr (Function _) = error "`cdr` is undefined for atomic arguments"
cdr (Cons _ b)   = b

cons :: Expr -> Expr -> Expr
cons = Cons

eq :: Expr -> Expr -> Expr
eq a b = if a == b then T else Nil

atom :: Expr -> Expr
atom (Cons _ _) = Nil
atom _          = T

null' :: Expr -> Expr
null' Nil = T
null' _   = Nil

pairlis :: Expr -> Expr -> Expr -> Expr
pairlis x y a
  | null' x == T = a
  | otherwise    = cons (cons (car x) (car y)) (pairlis (cdr x) (cdr y) a)

assoc :: Expr -> Expr -> Expr
assoc x a
  | eq (car $ car a) x == T = car a
  | otherwise               = assoc x (cdr a)

evcon :: Expr -> Expr -> Expr
evcon c a
  | null' (eval (car $ car c) a) == Nil = eval (car $ cdr $ car c) a
  | otherwise                           = evcon (cdr c) a

evlis :: Expr -> Expr -> Expr
evlis m a
  | null' m == T = Nil
  | otherwise    = cons (eval (car m) a) (evlis (cdr m) a)

apply :: Expr -> Expr -> Expr -> Expr
apply fn x a
  | atom fn == T =
      case fn of
        (Function CAR)  -> car $ car x
        (Function CDR)  -> cdr $ car x
        (Function CONS) -> cons (car x) (car $ cdr x)
        (Function ATOM) -> atom (car x)
        (Function EQQ)  -> eq (car x) (car $ cdr x)
        _               -> apply (eval fn a) x a
  | eq (car fn) (Function LAMBDA) == T = eval (car $ cdr $ cdr fn) (pairlis (car $ cdr fn) x a)
  | eq (car fn) (Function LABEL)  == T = apply (car $ cdr $ cdr fn) x (cons (cons (car $ cdr fn) (car $ cdr $ cdr fn)) a)

eval :: Expr -> Expr -> Expr
eval e a
  | atom e       == T = cdr (assoc e a)
  | atom (car e) == T =
      case car e of
        (Function QUOTE) -> car $ cdr e
        (Function COND)  -> evcon (cdr e) a
        _                -> apply (car e) (evlis (cdr e) a) a
  | otherwise         = apply (car e) (evlis (cdr e) a) a

evalquote :: Expr -> Expr -> Expr
evalquote fn x = apply fn x Nil

-- Test Cases
--   To run, paste `Input` expression into REPL. Match output with `Result` expression.

-- Input: evalquote (Cons (Function LAMBDA) (Cons (Cons (Atom (Identifier "X")) (Cons (Atom (Identifier "Y")) Nil)) (Cons (Cons (Function CONS) (Cons (Cons (Function CAR) (Cons (Atom (Identifier "X")) Nil)) (Cons (Atom (Identifier "Y")) Nil))) Nil))) (Cons (Cons (Atom (Identifier "A")) (Cons (Atom (Identifier "B")) Nil)) (Cons (Cons (Atom (Identifier "C")) (Cons (Atom (Identifier "D")) Nil)) Nil))
-- Result: Cons (Atom (Identifier "A")) (Cons (Atom (Identifier "C")) (Cons (Atom (Identifier "D")) Nil))
