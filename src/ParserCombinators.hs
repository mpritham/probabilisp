module ParserCombinators where

import Core
import Data.Char (isSpace)
import Prelude

newtype Parser a = Parser (String -> [(a, String)])

run :: Parser a -> String -> [(a, String)]
run (Parser f) = f

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

choice :: Parser a -> Parser a -> Parser a
choice p q =
  Parser
    ( \xs -> case run p xs of
        [] -> run q xs
        (x : _) -> [x]
    )

(<|>) :: Parser a -> Parser a -> Parser a
(<|>) = choice

sat :: (Char -> Bool) -> Parser Char
sat p =
  item >>= \x ->
    if p x then result x else zero

char :: Char -> Parser Char
char x = sat (== x)

digit :: Parser Char
digit = sat (\x -> '0' <= x && x <= '9')

lower :: Parser Char
lower = sat (\x -> 'a' <= x && x <= 'z')

upper :: Parser Char
upper = sat (\x -> 'A' <= x && x <= 'Z')

string :: String -> Parser String
string "" = result ""
string (x : xs) = char x >> string xs >> return (x : xs)

eof :: Parser ()
eof =
  Parser
    ( \xs ->
        case xs of
          [] -> [((), [])]
          _ -> []
    )

integer :: Parser String
integer = token $ many1plus digit

many :: Parser a -> Parser [a]
many p = many1plus p <|> return []

many1plus :: Parser a -> Parser [a]
many1plus p = do
  x <- p
  xs <- many p
  return (x : xs)

spaces :: Parser String
spaces = many (sat isSpace)

sepBy :: Parser a -> Parser b -> Parser [a]
sepBy p sep = sepBy1plus p sep <|> return []

sepBy1plus :: Parser a -> Parser b -> Parser [a]
sepBy1plus p sep = do
  x <- p
  xs <- many (sep >> p)
  return (x : xs)

option :: a -> Parser a -> Parser a
option x p = p <|> return x

optionMaybe :: Parser a -> Parser (Maybe a)
optionMaybe p = option Nothing (Just <$> p)

between :: Parser open -> Parser close -> Parser a -> Parser a
between open close p = do
  _ <- open
  x <- p
  _ <- close
  return x

-- ignore leading whitespace
token :: Parser a -> Parser a
token p = spaces >> p

oneOf :: [Char] -> Parser Char
oneOf [] = undefined
oneOf [c] = char c
oneOf (c : cs) = foldl (\acc x -> acc <|> (char x)) (char c) cs

idP :: Parser String
idP =
  let identFirst = oneOf $ "-*+/:?><=!" Prelude.++ ['a' .. 'z'] Prelude.++ ['A' .. 'Z']
      identRest = identFirst <|> digit
   in do
        x <- identFirst
        xs <- (many identRest)
        return (x : xs)

--- ### Value parsers

symP :: Parser Val
symP = Symbol <$> idP

pairList :: [Val] -> Val
pairList [] = Nil
pairList (x : xs) = Pair x (pairList xs)

pairUnfold :: [Val] -> Val -> Val
pairUnfold [] tail = tail
pairUnfold (x : xs) tail = Pair x (pairUnfold xs tail)

-- Parses list and dotted list
listRestP :: Parser Val
listRestP = do
  exprs <- rawExprP `sepBy` spaces
  maybeTail <- optionMaybe $ char '.' >> spaces >> rawExprP
  return $ case maybeTail of
    Just tail -> pairUnfold exprs tail
    Nothing -> pairUnfold exprs Nil

-- Parses lists
listP :: Parser Val
listP = do
  char '(' >> spaces
  result <- listRestP
  spaces >> char ')'
  return result

numP :: Parser Val
numP = Number . read <$> integer

boolP :: Parser Val
boolP = char '#' >> boolLitP
  where
    boolLitP =
      ((Boolean <$> const True) <$> char 't')
        <|> ((Boolean <$> const False) <$> char 'f')

quoteP :: Parser Val
quoteP = char '\'' >> (\x -> pairUnfold [Symbol "quote", x] Nil) <$> rawExprP

quasiquoteP :: Parser Val
quasiquoteP = char '`' >> (\x -> pairUnfold [Symbol "quasiquote", x] Nil) <$> rawExprP

unquoteP :: Parser Val
unquoteP = char ',' >> (\x -> pairUnfold [Symbol "unquote", x] Nil) <$> rawExprP

rawExprP :: Parser Val
rawExprP =
  numP
    <|> symP
    <|> boolP
    <|> quoteP
    <|> quasiquoteP
    <|> unquoteP
    <|> listP

exprP :: Parser Val
exprP = between spaces spaces rawExprP <* eof
