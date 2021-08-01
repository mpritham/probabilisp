module Main where

import Prob (uniform)

-- main :: IO ()
-- main = do
--   putStrLn "Welcome to your probabilisp interpreter!!!"


import Control.Monad
import Control.Monad.Except
import Control.Monad.State
import Core
import Eval
import Runtime
import ParserCombinators
import System.IO (hFlush, hGetLine, hPutStr, hPutStrLn, stdin, stdout)
-- import Text.ParserCombinators.Parsec hiding (Parser, State)
import Prelude hiding (lookup)

--- ### REPL

printLn :: String -> IO ()
printLn str = hPutStrLn stdout str >> hFlush stdout

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
    (_, s):_ -> print s
    [] -> print "ohno"
  repl env -- Loop with old env

main :: IO ()
main = repl runtime
