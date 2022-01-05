data Symbol = String String
            | Integer Int
            | Float Double
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
eq Nil          Nil          = T
eq T            T            = T
eq (Atom a)     (Atom b)     = if a == b then T else Nil
eq (Function f) (Function g) = if f == g then T else Nil
eq _            _            = Nil

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
  | eq (car (car a)) x == T = car a
  | otherwise               = assoc x (cdr a)

evcon :: Expr -> Expr -> Expr
evcon c a
  | null' (eval (car (car c)) a) == Nil = eval ((car . cdr . car) c) a
  | otherwise                           = evcon (cdr c) a

evlis :: Expr -> Expr -> Expr
evlis m a
  | null' m == T = Nil
  | otherwise    = cons (eval (car m) a) (evlis (cdr m) a)

apply :: Expr -> Expr -> Expr -> Expr
apply fn x a
  | atom fn == T =
      case fn of
        (Function CAR)  -> car (car x)
        (Function CDR)  -> cdr (car x)
        (Function CONS) -> cons (car x) ((car . cdr) x)
        (Function ATOM) -> atom (car x)
        (Function EQQ)  -> eq (car x) ((car . cdr) x)
        _               -> apply (eval fn a) x a
  | eq (car fn) (Function LAMBDA) == T = eval ((car. cdr . cdr) fn) (pairlis ((car . cdr) fn) x a)
  | eq (car fn) (Function LABEL)  == T = apply ((car . cdr . cdr) fn) x (cons (cons ((car . cdr) fn) ((car . cdr . cdr) fn)) a)
  | otherwise                          = undefined

eval :: Expr -> Expr -> Expr
eval e a
  | atom e       == T = cdr (assoc e a)
  | atom (car e) == T =
      case car e of
        (Function QUOTE) -> car (cdr e)
        (Function COND)  -> evcon (cdr e) a
        _                -> apply (car e) (evlis (cdr e) a) a
  | otherwise         = apply (car e) (evlis (cdr e) a) a

evalquote fn x = apply fn x Nil

-- Test Cases

-- Input: evalquote (Cons (Function LAMBDA) (Cons (Cons (Atom (String "X")) (Cons (Atom (String "Y")) Nil)) (Cons (Cons (Function CONS) (Cons (Cons (Function CAR) (Cons (Atom (String "X")) Nil)) (Cons (Atom (String "Y")) Nil))) Nil))) (Cons (Cons (Atom (String "A")) (Cons (Atom (String "B")) Nil)) (Cons (Cons (Atom (String "C")) (Cons (Atom (String "D")) Nil)) Nil))
-- Result: Cons (Atom (String "A")) (Cons (Atom (String "C")) (Cons (Atom (String "D")) Nil))
