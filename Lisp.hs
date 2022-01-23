module Lisp where

data Atom = Nil
          | T
          | Identifier String
          | String String
          | Integer Int
          deriving (Eq)

instance Show Atom where
  show Nil            = "nil"
  show T              = "t"
  show (Identifier s) = s
  show (String s)     = "\"" ++ s ++ "\""
  show (Integer i)    = show i

data Expr = Atom Atom
          | Cons Expr Expr
          deriving (Eq)

instance Show Expr where
  show (Atom a)   = show a
  show (Cons a b) = "(" ++ show a ++ " " ++ show b ++ ")"

cons :: Expr -> Expr -> Expr
cons = Cons

car :: Expr -> Expr
car (Atom _)   = Atom Nil
car (Cons a _) = a

cdr :: Expr -> Expr
cdr (Atom _)   = Atom Nil
cdr (Cons _ b) = b

eq :: Expr -> Expr -> Expr
eq (Atom a) (Atom b) = if a == b then Atom T else Atom Nil
eq _        _        = Atom Nil

atom :: Expr -> Expr
atom (Atom _) = Atom T
atom _        = Atom Nil

null' :: Expr -> Expr
null' (Atom Nil) = Atom T
null' _          = Atom Nil

pairlis :: Expr -> Expr -> Expr -> Expr
pairlis x y a
  | null' x == Atom T = a
  | otherwise         = cons (cons (car x) (car y)) (pairlis (cdr x) (cdr y) a)

assoc :: Expr -> Expr -> Expr
assoc x a
  | null' a            == Atom T = error "Symbol is not defined"
  | eq (car $ car a) x == Atom T = car a
  | otherwise                    = assoc x (cdr a)

evcon :: Expr -> Expr -> Expr
evcon c a
  | null' (eval (car $ car c) a) == Atom T = eval (car $ cdr $ car c) a
  | otherwise                              = evcon (cdr c) a

evlis :: Expr -> Expr -> Expr
evlis m a
  | null' m == Atom T = Atom Nil
  | otherwise         = cons (eval (car m) a) (evlis (cdr m) a)

apply :: Expr -> Expr -> Expr -> Expr
apply fn x a
  | atom fn == Atom T =
    case fn of
      (Atom (Identifier "car"))  -> car $ car x
      (Atom (Identifier "cdr"))  -> cdr $ car x
      (Atom (Identifier "cons")) -> cons (car x) (car $ cdr x)
      (Atom (Identifier "atom")) -> atom (car x)
      (Atom (Identifier "eq"))   -> eq (car x) (car $ cdr x)
      _                          -> apply (eval fn a) x a
  | eq (car fn) (Atom (Identifier "lambda")) == Atom T =
    eval (car $ cdr $ cdr fn) (pairlis (car $ cdr fn) x a)
  | eq (car fn) (Atom (Identifier "label"))  == Atom T =
    apply (car $ cdr $ cdr fn) x (cons (cons (car $ cdr fn) (car $ cdr $ cdr fn)) a)
  | otherwise = eval (Cons fn x) a

eval :: Expr -> Expr -> Expr
eval e a
  | atom e       == Atom T = cdr (assoc e a)
  | atom (car e) == Atom T =
    case car e of
      (Atom (Identifier "quote")) -> car $ cdr e
      (Atom (Identifier "cond"))  -> evcon (cdr e) a
      _                           -> apply (car e) (evlis (cdr e) a) a
  | otherwise              = apply (car e) (evlis (cdr e) a) a

evalquote :: Expr -> Expr -> Expr
evalquote fn x = apply fn x (Atom Nil)
