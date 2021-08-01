module Eval where

import Core
import Control.Monad.Except
import qualified Data.HashMap.Strict as H (HashMap, insert, lookup, empty, fromList, union)
import Control.Monad.State
import Prelude

-- ### Evaluation helpers

-- Evaluates a symbol to string
-- Throws an error if value is not a symbol
-- Examples:
--   getSym (Symbol "x")  ==> "x"
--   getSym (Number 1)    ==> Not a symbol: x
getSym :: Val -> EvalState String
getSym (Symbol x) = return x
getSym         v  = throwError $ NotASymbol v

--
-- `let` and `let*`
getBinding :: Val -> EvalState (String, Val)
getBinding (Pair c (Pair e Nil)) = liftM2 (,) (getSym c) (eval e)
getBinding v = throwError $ NotAListOfTwo v

-- Evaluates a list of two to a tuple
-- Throws an error if value is not a list of two
-- This is useful in special form `cond`, since each clause
-- is expected to be exactly a two-element list
getListOf2 :: Val -> EvalState (Val, Val)
getListOf2 (Pair c (Pair e Nil)) = return (c, e)
getListOf2 v = throwError $ NotAListOfTwo v

-- Evaluates a value representing a list into an actual list
getList :: Val -> EvalState [Val]
getList Nil = return []
getList (Pair v1 v2) =
  do xs <- getList v2
     return (v1 : xs)
getList e = throwError $ InvalidSpecialForm "special" e

--- ### Keywords

-- When evaluating special forms, a list form starting with a keyword
-- is expected to match the special form syntax.
keywords :: [String]
keywords = [ "define"
           , "lambda"
           , "cond"
           , "let"
           , "let*"
           ]

-- ### The monadic evaluator
-- Unlike evaluators in previous MPs, `eval` does not take any environment!
-- This is because the environment is encapsulated in the `EvalState` monad.
-- To access the environment, all you have to do is `get`, `modify` or `put`!
eval :: Val -> EvalState Val

-- Self-evaluating expressions
eval v@(Number _) = return v
eval v@(Boolean _) = return v

-- Symbol evaluates to the value bound to it
eval (Symbol sym) =
  do
    env <- get
    case H.lookup sym env of
      Nothing -> throwError $ UndefSymbolError ("Undefined symbol " ++ sym)
      Just v -> return v

-- Function closure is also self-evaluating
eval v@(Func _ _ _) = return v

-- We check to see if the pair is a "proper list". If it is,
-- then we try to evaluate it, as one of the following forms:
-- 1. Special form (`define`, `let`, `let*`, `cond`, `quote`, `quasiquote`,
--    `unquote`, `define-macro`, ...)
-- 2. Macro expansion (Macro)
-- 3. Function application (Func)
-- 4. Primitive function application (PrimFunc)
eval expr@(Pair v1 v2) = case flattenList expr of
  Left _ -> throwError $ InvalidExpression expr
  Right lst -> evalList lst where
    --- Evaluator for forms
    invalidSpecialForm :: String -> EvalState e
    invalidSpecialForm frm = throwError $ InvalidSpecialForm frm expr

    evalList :: [Val] -> EvalState Val

    evalList [] = throwError $ InvalidExpression expr

    -- quote
    evalList [Symbol "quote", e] = return e

    -- unquote (illegal at surface evaluation)
    evalList [Symbol "unquote", e] = throwError $ UnquoteNotInQuasiquote e

    -- quasiquote
    evalList [Symbol "quasiquote", e] = evalQuasi 1 e where
      evalQuasi :: Int -> Val -> EvalState Val
      evalQuasi 0 (Pair (Symbol "unquote") v) = throwError $ UnquoteNotInQuasiquote v
      evalQuasi 1 (Pair (Symbol "unquote") (Pair v Nil)) = eval v
      evalQuasi n (Pair (Symbol "quasiquote") (Pair v Nil)) =
        do v' <- evalQuasi (n+1) v
           return $ Pair (Symbol "quasiquote") (Pair v' Nil)
      evalQuasi n (Pair (Symbol "unquote") (Pair v Nil)) =
        do v' <- evalQuasi (n-1) v
           return $ Pair (Symbol "unquote") (Pair v' Nil)
      evalQuasi n (Pair x y) = Pair <$> evalQuasi n x <*> evalQuasi n y
      evalQuasi _ v = return v

    -- cond
    evalList ((Symbol "cond"):c:cs) =
      do
        (condition, retVal) <- getListOf2 c
        case condition of
          Symbol "else" ->
            case cs of
              [] -> eval retVal
              _ -> invalidSpecialForm "else"
          _ ->
            do
              evaled_cond <- eval condition
              case evaled_cond of
                Boolean False ->
                  case cs of
                    [] -> return Void
                    cs -> evalList (Symbol "cond":cs)
                _ -> eval retVal

    -- let
    evalList [Symbol "let", bindings, body] =
      do
        origEnv <- get
        bindingList <- getList bindings
        bindingVals <- mapM getBinding bindingList
        modify $ H.union (H.fromList bindingVals)
        evaled <- eval body
        modify $ const origEnv
        return evaled

    -- lambda
    evalList [Symbol "lambda", args, body] =
      do
        env <- get
        argList <- getList args
        (\argVal -> Func argVal body env) <$> mapM getSym argList


    -- define function
    evalList [Symbol "define", Pair (Symbol fname) args, body] =
      do
        env <- get
        argList <- getList args
        val <- (\argVal -> Func argVal body env) <$> mapM getSym argList
        modify $ H.insert fname val
        return Void

    -- define variable
    -- to match the syntax
    evalList [Symbol "define", Symbol var, body] =
      do
        env <- get
        evaled <- eval body
        modify $ H.insert var evaled
        return Void

    -- invalid use of keyword, throw a diagnostic
    evalList (Symbol sym : _) | elem sym keywords = invalidSpecialForm sym

    -- application
    evalList (fexpr:args) =
      do f <- eval fexpr
         apply f args

eval val = throwError $ InvalidExpression val

-- Function application
apply :: Val -> [Val] -> EvalState Val
apply (Func params body env) args =
  do
    origEnv <- get
    argVals <- mapM eval args
    modify $ H.union (H.union (H.fromList (zip params argVals)) env)
    evaled <- eval body
    modify $ const origEnv
    return evaled

apply (PrimFunc p) args =
  do argVals <- mapM eval args
     p argVals
  -- Other values are not applicable
apply f args = throwError $ CannotApply f args
