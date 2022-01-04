-- S-expressions

type Symbol = String

data Expr = Nil
          | Atom Symbol
          | Cons Expr Expr
          deriving (Show)

-- Elementary functions of LISP
--   car
--   cdr
--   cons
--   eq
--   atom

car :: Expr -> Expr
car Nil        = Nil
car (Atom a)   = error "`car` is undefined for atomic arguments"
car (Cons a _) = a

cdr :: Expr -> Expr
cdr Nil        = Nil
cdr (Atom a)   = error "`cdr` is undefined for atomic arguments"
cdr (Cons _ b) = b

cons :: Expr -> Expr -> Expr
cons = Cons

eq :: Expr -> Expr -> Bool
eq Nil        Nil        = True
eq (Atom a)   (Atom b)   = a == b
eq (Cons _ _) _          = error "`eq` is undefined for non-atomic arguments"
eq _          (Cons _ _) = error "`eq` is undefined for non-atomic arguments"
eq _          _          = False

atom :: Expr -> Bool
atom Nil        = True
atom (Atom _)   = True
atom (Cons _ _) = False
