module Core where

import Control.Monad
import Control.Monad.Error

import Data.List

import Types

-- Evalution

evalExprs :: SchemeEnv -> [SchemeVal] -> SchemeM SchemeVal
evalExprs env (expr : []) = evalExpr env expr
evalExprs env (expr : exprs) = evalExpr env expr >> evalExprs env exprs
evalExprs env [] = throwError $ strMsg "empty body"

evalExpr :: SchemeEnv -> SchemeVal -> SchemeM SchemeVal
evalExpr env expr@(Pair car cdr) = do
  args <- fromSchemeVal cdr
  head <- evalExpr env car
  case head of
    Proc proc -> mapM (evalExpr env) args >>= proc 
    Syntax syn -> syn env args
    _ -> throwError $ strMsg ("invalid application: " ++ show expr)
evalExpr env (Symbol var) = getVar env var
evalExpr env val = return val

primitiveEnv :: SchemeM SchemeEnv
primitiveEnv = extendEnv nullEnv `liftM` makeFrame primitives
  where primitives = primitiveSyntax ++ primitiveProcs

-- Primitive Syntax

primitiveSyntax = [("lambda", Syntax lambda)]

lambda :: SchemeSyntax 
lambda env (formals : body) = return . Proc $ \args -> do
  bindings <- bind formals args
  env <- extendEnv env `liftM` makeFrame bindings
  evalExprs env body
  where bind (Pair (Symbol s) rest) (val : args) = liftM ((s, val) :) $ bind rest args
        bind (Pair (Symbol s) rest) [] = throwError $ strMsg "too few arguments"
        bind (Symbol s) args = return [(s, toSchemeVal args)]
        bind Nil (_:_) = throwError $ strMsg "too many arguments"
        bind Nil [] = return []
        bind _ _ = throwError $ strMsg "malformed lambda"
lambda env  _ = throwError $ strMsg "malformed lambda"

-- Primitive Procedures

primitiveProcs = [("display", Proc display),
                  ("+", Proc $ toSchemeNumericOp add),
                  ("-", Proc $ toSchemeNumericOp sub),
                  ("*", Proc $ toSchemeNumericOp mul)]

display :: SchemeProc
display (val : []) = liftIO . liftM toSchemeVal $ print val
display (_:_) = throwError $ strMsg "too many arguments"
display [] = throwError $ strMsg "too few arguments"

type NumericOp = [Integer] -> Either SchemeError Integer

toSchemeNumericOp :: NumericOp -> SchemeProc
toSchemeNumericOp op ns = mapM fromSchemeVal ns >>= liftError . liftM toSchemeVal . op

add :: NumericOp
add = return . foldl' (+) 0

sub :: NumericOp
sub ns@(_:_:_) = return $ foldl1' (-) ns
sub (n : []) = return $ negate n
sub _ = throwError $ strMsg "procedure requires at least one argument"

mul :: NumericOp
mul = return . foldl' (*) 1
