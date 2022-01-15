module Parser where

data Error i = EOF
             | Unexpected i
             | Empty
             deriving (Show, Eq)

newtype Parser i a = Parser
  { runParser :: [i] -> Either [Error i] (a, [i])
  }

instance Functor (Parser i) where
  fmap f (Parser parse) = Parser $ \input ->
    case parse input of
      Left error    -> Left error
      Right (x, xs) -> Right (f x, xs)

instance Applicative (Parser i) where
  pure x = Parser $ \input -> Right (x, input)

  Parser f <*> Parser p = Parser $ \input -> do
    (f', xs) <- f input
    (x, xs') <- p xs
    pure (f' x, xs')

instance Monad (Parser i) where
  return = pure

  (Parser p) >>= f = Parser $ \input -> do
    (x, xs) <- p input
    runParser (f x) xs

satisfy :: (i -> Bool) -> Parser i i
satisfy pred = Parser $ \input ->
  case input of
    [] -> Left [EOF]
    x:xs
      | pred x    -> Right (x, xs)
      | otherwise -> Left [Unexpected x]

char :: Eq a => a -> Parser a a
char c = satisfy (== c)
