module Main where

import Control.Monad
import Control.Monad.Except
import Control.Monad.State
import Core
import Eval
import ParserCombinators
import Prob (uniform)
import Runtime
import System.IO (hFlush, hGetLine, hPutStr, hPutStrLn, stdin, stdout)
import Prelude hiding (lookup)

--- ### REPL

printLn :: String -> IO ()
printLn str = hPutStrLn stdout str >> hFlush stdout

prelude :: Env -> Env
prelude env =
  foldl
    (flip aux)
    env
    [ -- Add probabilisp commands that you want to run before repl starts
      "(define die (uniform '(1 2 3 4 5 6)))",
      "(define (dice n) (cond ((= n 0) (uniform '( '()))) (else (concatP die (dice (- n 1))))))",
      "(define (filter pred xs) (cond ((null? xs) xs) ((pred (car xs)) (cons (car xs) (filter pred (cdr xs)))) (else (filter pred (cdr xs)))))",
      "(define (length xs) (cond ((null? xs) 0) (else (+ 1 (length (cdr xs))))))",
      "(define (eq6? x) (= 6 x))",
      "(define (listN n) (cond ((= 0 n) '()) (else (cons n (listN (- n 1))))))"
    ]
  where
    aux command env =
      case run rawExprP command of
        [(expr, s)] ->
          case runExcept $ runStateT (eval expr) env of
            Left err -> env
            Right (Void, env') -> env'
            Right (val, env') -> env'
        _ -> env

repl :: Env -> IO ()
repl env = do
  putStr "probabilisp> "
  l <- getLine -- Read
  case run rawExprP l of -- Parse
    [(expr, s)] ->
      case runExcept $ runStateT (eval expr) env of -- Eval
        Left err -> print err
        Right (Void, env') -> repl env'
        Right (val, env') ->
          do
            print val
            repl env'
    (_, s) : _ -> print s
    [] -> print "ohno"
  repl env -- Loop with old env

main :: IO ()
main =
  let runtime' = prelude runtime
   in repl runtime'
