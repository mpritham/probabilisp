# CS421 - Final Project - Probabilisp

**Authors:**

Pritham Marupaka, pritham2@illinois.edu

Zi Mo (Andy) Su, zimoms2@illinois.edu

## Overview

Probabilistic reasoning is used in many domains, including financial stock
prediction, intrusion detection for cybersecurity, and image recognition.
Probabilistic programming is a paradigm in which a user specifies parameters for
their desired probability models and inference is performed automatically.
Probabalisp is a simple Lisp-like domain specific probabilistic programming
language. The primary goal of Probabalisp is to make probability primitives
accessible to programmers, while abstracting away the details of the
mathematical computation. To this end, Probabalisp is able to represent problems that would require several lines of dense math library code with simple functions, e.g. `uniform`, `sample`, and `select`. 

## Implementation

### Major tasks and capabilities
1. A monadic parser combinator library was implemented to lex Probabilisp tokens, and parse Probabilisp expressions. The parser's main abstraction is as follows:

```haskell
newtype Parser a = Parser (String -> [(a, String)])

result :: a -> Parser a
result v = Parser $ \inp -> [(v, inp)]

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

```
2. Several 

3. The Probabilisp interpreter is a subset of the Scheme interpreter. The interpreter supports:
    - Functions
    - Lambda expressions
    - Integer and Float arithmetic
    - List processing functions such as `length` and `sort`
    - Conditional evaluation via `cond`


### Components of the project

### Project Status

#### What works well
#### What works partially
#### Unimplemented functionality
#### Comparison with project proposal

## Tests
## Listing

```lisp:example/dice.lisp
```

```lisp:example/marbles.lisp
```

```lisp:example/cards.lisp
```
