module Parser where

newtype Parser a = Parser (String -> [(a, String)])

result :: a -> Parser a
result v = Parser $ \inp -> [(v, inp)]

item :: Parser Char
item = Parser $ \inp ->
  case inp of
    [] -> []
    (x : xs) -> [(x, xs)]

bind :: Parser a -> (a -> Parser b) -> Parser b
bind (Parser p) f =
  Parser $ \inp -> concat [app (f x) inp' | (x, inp') <- p inp]
  where
    app (Parser q) = q

instance Functor Parser where
  fmap f (Parser a) = Parser (\inp -> map (\(x, y) -> (f x, y)) $ a inp)

instance Applicative Parser where
  pure = result
  Parser f <*> Parser a =
    Parser
      ( \inp ->
          [ (fn x, inp'') | (x, inp') <- a inp, (fn, inp'') <- f inp'
          ]
      )

instance Monad Parser where
  (>>=) = bind
  return = result

class Monad m => MonadOPlus m where
  zero :: m a
  (++) :: m a -> m a -> m a

instance MonadOPlus Parser where
  zero = Parser (\_ -> [])
  (Parser p) ++ (Parser q) = Parser (\inp -> (p inp) Prelude.++ (q inp))

seq :: Parser a -> Parser b -> Parser (a, b)
seq p q =
  p >>= \x ->
    q >>= \y ->
      result (x, y)

sat :: (Char -> Bool) -> Parser Char
sat p =
  item >>= \x ->
    if p x then result x else zero

char :: Char -> Parser Char
char x = sat (\y -> x == y)

digit :: Parser Char
digit = sat (\x -> '0' <= x && x <= '9')

lower :: Parser Char
lower = sat (\x -> 'a' <= x && x <= 'z')

upper :: Parser Char
upper = sat (\x -> 'A' <= x && x <= 'Z')

string :: String -> Parser String
string "" = result ""
string (x : xs) =
  char x >>= \_ ->
    string xs >>= \_ ->
      result (x : xs)
