module Core where

import Control.Monad
import Control.Monad.Error

import Data.List

import Types

-- Evalution

evalExprs :: SchemeEnv -> [SchemeVal] -> SchemeM SchemeVal
evalExprs env [] = throwError $ strMsg "empty body"
evalExprs env [expr] = evalExpr env expr
evalExprs env (expr : exprs) = evalExpr env expr >> evalExprs env exprs

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

primitiveSyntax = [("quote",  Syntax quote),
                   ("lambda", Syntax lambda),
                   ("if",     Syntax if'),
                   ("set!",   Syntax set)]

lambda :: SchemeSyntax 
lambda env (formals : body) = return . Proc $ \args -> do
  bindings <- bind formals args
  env <- extendEnv env `liftM` makeFrame bindings
  evalExprs env body
  where bind (Pair (Symbol var) rest) (val : args) = liftM ((var, val) :) $ bind rest args
        bind (Pair (Symbol _) _) [] = throwError $ strMsg "too few arguments"
        bind (Symbol var) args = return [(var, toSchemeVal args)]
        bind Nil (_:_) = throwError $ strMsg "too many arguments"
        bind Nil [] = return []
        bind _ _ = throwError $ strMsg "malformed lambda"
lambda env  _ = throwError $ strMsg "malformed lambda"

quote :: SchemeSyntax
quote _ [datum] = return datum
quote _ _ = throwError $ strMsg "malformed quote"

if' :: SchemeSyntax
if' env [test, expr, expr'] = evalExpr env test >>= fromSchemeVal >>=
                              evalExpr env . bool expr expr' 
  where bool a a' b = if b then a else a'
if' _ _ = throwError $ strMsg "malformed if"

set :: SchemeSyntax
set env [(Symbol var), expr] = evalExpr env expr >>= setVar env var >>
                               return Unspecified
set _ _ = throwError $ strMsg "malformed set"

-- Primitive Procedures

primitiveProcs = [("display", Proc display),
                  ("+", Proc $ toSchemeNumericOp add),
                  ("-", Proc $ toSchemeNumericOp sub),
                  ("*", Proc $ toSchemeNumericOp mul)]

display :: SchemeProc
display [val] = liftIO $ print val >> return Unspecified
display [] = throwError $ strMsg "too few arguments"
display _ = throwError $ strMsg "too many arguments"

type NumericOp = [Integer] -> Either SchemeError Integer

toSchemeNumericOp :: NumericOp -> SchemeProc
toSchemeNumericOp op ns = mapM fromSchemeVal ns >>=
                          liftError . liftM toSchemeVal . op

add :: NumericOp
add = return . foldl' (+) 0

sub :: NumericOp
sub [] = throwError $ strMsg "procedure requires at least one argument"
sub [n] = return $ negate n
sub ns = return $ foldl1' (-) ns

mul :: NumericOp
mul = return . foldl' (*) 1
