module Prob where

import Data.Function (on)
import Data.List (delete, (\\))
import Text.Printf (printf)

-- Types

newtype Dist a = D {unD :: [(a, Probability)]}
  deriving (Eq, Show)

compress :: (Show a, Eq a) => Dist a -> String
compress d = show aux
  where
    aux =
      foldr
        ( \(x, p) acc ->
            case lookup x acc of
              Just p' ->
                map
                  ( \(y, q) ->
                      if y == x then (x, p + p') else (y, q)
                  )
                  acc
              Nothing -> (x, p) : acc
        )
        []
        (unD d)

type Probability = Float

type Spread a = [a] -> Dist a

type Event a = a -> Bool

type Trans a = a -> Dist a

-- Distributions

uniform :: Spread a
uniform xx =
  let count = fromIntegral (length xx) :: Float
      p = 1.0 / count
   in D (map (\x -> (x, p)) xx)

enum :: [Probability] -> [a] -> Dist a
enum pp xx =
  D (zip xx pp)

-- Determine the probability of an event across a distribution

(??) :: Event a -> Dist a -> Probability
(??) pred (D d) = foldr aux 0 d
  where
    aux (x, p) acc
      | pred x = acc + p
      | otherwise = acc

-- Join two distributions

join :: (a -> b -> c) -> Dist a -> Dist b -> Dist c
join f (D dx) (D dy) = D [(f x y, px * py) | (x, px) <- dx, (y, py) <- dy]

prod :: Dist a -> Dist b -> Dist (a, b)
prod = join (,)

-- Functor, Applicative, Monad

instance Functor Dist where
  fmap f (D d) = D [(f x, p) | (x, p) <- d]

instance Applicative Dist where
  pure x = D [(x, 1)]
  (D f) <*> (D d) = D [(g x, p * q) | (x, p) <- d, (g, q) <- f]

instance Monad Dist where
  return x = D [(x, 1)]
  (D d) >>= f = D [(y, p * q) | (x, p) <- d, (y, q) <- unD (f x)]

mapD :: (a -> b) -> Dist a -> Dist b
mapD = fmap

certainly :: a -> Dist a
certainly = return

certainlyT :: (a -> a) -> Trans a
certainlyT f = certainly . f

-- Monadic composition

(>@>) :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c
f >@> g = (>>= g) . f

-- Monadic composition of a list of functions

sequ :: Monad m => [a -> m a] -> a -> m a
sequ = foldl (>@>) return

-- Select (select without putting back)

selectOne :: Eq a => [a] -> Dist (a, [a])
selectOne c = uniform [(v, delete v c) | v <- c]

selectMany :: Eq a => Int -> [a] -> Dist ([a], [a])
selectMany 0 c = return ([], c)
selectMany n c = do
  (x, c1) <- selectOne c
  (xs, c2) <- selectMany (n -1) c1
  return (x : xs, c2)

select :: Eq a => Int -> [a] -> Dist [a]
select n = mapD (reverse . fst) . selectMany n

-- Sample (select and put back)

sampleOne :: Eq a => [a] -> Dist (a, [a])
sampleOne c = uniform [(v, c) | v <- c]

sampleMany :: Eq a => Int -> [a] -> Dist ([a], [a])
sampleMany 0 c = return ([], c)
sampleMany n c = do
  (x, c1) <- sampleOne c
  (xs, c2) <- sampleMany (n -1) c1
  return (x : xs, c2)

sample :: Eq a => Int -> [a] -> Dist [a]
sample n = mapD (reverse . fst) . sampleMany n

{-
Probabilisp Example 1: Dice

(define die (uniform '(1 2 3 4 5 6)))
(define (dice n) (cond ((= n 0) (uniform '( '()))) (else (concatP die (dice (- n 1))))))
(define (filter pred xs) (cond ((null? xs) xs) ((pred (car xs)) (cons (car xs) (filter pred (cdr xs)))) (else (filter pred (cdr xs)))))
(define (length xs) (cond ((null? xs) 0) (else (+ 1 (length (cdr xs))))))
(define (eq6? x) (= 6 x))
(define (pred xs) (>= (length (filter eq6? xs)) 2))
(?? pred (dice 4))
-}

{-
Probabilisp Example 2: Marbles

(define (pred xs) (let ((i1 (car xs)) (i2 (car (cdr xs))) (i3 (car (cdr (cdr xs))))) (and (eq? i1 'r) and (eq? i2 'g) (eq? i3 'b)))))
(?? pred (select 3 '('r 'r 'g 'g 'b)))
-}

{-
Probabilisp Example 3: Cards

(define (listN n) (cond ((= 0 n) '()) (else (cons n (listN (- n 1))))))
(define (predH xs) (cond ((<= (length xs) 1) #f) ((= (car xs) (car (cdr xs))) #t) (else (predH (cdr xs))))))
(define (pred xs) (predH (sort xs)))
(?? pred (sample 2 (listN 52)))
-}

{-
Haskell Example 1: Dice

die :: Dist Int
die = uniform [1 .. 6]

dice :: Int -> Dist [Int]
dice 0 = certainly []
dice n = join (:) die (dice (n - 1))

> ((>=2) . length . filter (==6)) ?? dice 4
0.13194445
-}

{-
Haskell Example 2: Marbles
> (==['R','G','B']) ?? select 3 ['R','R','G','G','B']
6.666667e-2
-}

{-
Haskell Example 3.1: Monty Hall

data Outcome = Win | Lose
  deriving (Eq, Show)

data Door = A | B | C
  deriving (Eq, Show)

firstChoice :: Dist Outcome
firstChoice = uniform [Win, Lose, Lose]

-- switch :: Trans Outcome
-- switch Win = certainly Lose
-- switch Lose = certainly Win

> firstChoice
[(Win,0.33333334),(Lose,0.6666667)]

> firstChoice >>= switch
[(Lose,0.33333334),(Win,0.6666667)]

(define firstChoice (uniform '('win 'lose 'lose)))
(define switch

)
-}

{-
Haskell Example 3.2: Monty Hall

doors :: [Door]
doors = [A, B, C]

data State = Doors {prize :: Door, chosen :: Door, opened :: Door}

start :: State
start = Doors {prize = u, chosen = u, opened = u} where u = undefined

hide :: Trans State
hide s = uniform [s {prize = d} | d <- doors]

choose :: Trans State
choose s = uniform [s {chosen = d} | d <- doors]

open :: Trans State
open s = uniform [s {opened = d} | d <- doors \\ [prize s, chosen s]]

type Strategy = Trans State

switch :: Strategy
switch s = uniform [s {chosen = d} | d <- doors \\ [chosen s, opened s]]

stay :: Strategy
stay = certainlyT id

game :: Strategy -> Trans State
game s = sequ [hide, choose, open, s]

result :: State -> Outcome
result s = if chosen s == prize s then Win else Lose

eval :: Strategy -> Dist Outcome
eval s = mapD result (game s start)

> eval stay
[(Lose,0.6666667),(Win,0.33333334)]

> eval switch
[(Win,0.6666667),(Lose,0.33333334)]
-}
