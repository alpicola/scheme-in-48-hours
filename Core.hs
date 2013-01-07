module Core where

import Control.Monad
import Control.Monad.Error
import Control.Arrow

import Data.List
import Data.Maybe

import Types

-- Evalution

evalExprs :: SchemeEnv -> [SchemeVal] -> SchemeM SchemeVal
evalExprs env [] = throwError $ strMsg "empty body"
evalExprs env [expr] = evalExpr env expr
evalExprs env (expr : exprs) = evalExpr env expr >> evalExprs env exprs

evalExpr :: SchemeEnv -> SchemeVal -> SchemeM SchemeVal
evalExpr env expr@(Pair car cdr) = do
  args <- catchError (fromSchemeVal cdr)
                     (const $ throwError $ SyntaxError msg expr)
  if isSymbol car
    then do
       val <- fromSchemeVal car >>= getVar env
       case val of
         Syntax syn -> syn env args
         _ -> call val args
    else evalExpr env car >>= flip call args
  where
    call (Proc proc) args = mapM (evalExpr env) args >>= proc
    call val _ = throwError $ TypeError "procedure" val
    msg = "proper list required for procedure call or macro use"
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

lambda env (formals : body) = do
  formals <- parse formals
  return . Proc $ \args -> do 
    bindings <- bind formals args
    env <- extendEnv env `liftM` makeFrame bindings
    evalExprs env body
  where
    parse (Pair (Symbol var) cdr) = liftM (first (var:)) $ parse cdr
    parse (Symbol var) = return ([], Just var)
    parse Nil = return ([], Nothing)
    parse _ = malformedSyntax "lambda" (formals : body)
    bind (vars, dotted) args = bind' vars args
      where num = show $ length vars
            bind' (var : vars) (val : vals) = liftM ((var, val) :) $ bind' vars vals
            bind' (_:_) [] = throwError $ ArgumentError num args
            bind' [] [] = return []
            bind' [] vals = maybe (throwError $ ArgumentError num args)
                                  (\var -> return [(var, toSchemeVal vals)]) dotted
lambda _ args = malformedSyntax "lambda" args

quote :: SchemeSyntax
quote _ [datum] = return datum
quote _ args = malformedSyntax "quote" args

if' :: SchemeSyntax
if' env [test, expr, expr'] = evalExpr env test >>= fromSchemeVal >>=
                              evalExpr env . bool expr expr' 
  where bool a a' b = if b then a else a'
if' _ args = malformedSyntax "if" args

set :: SchemeSyntax
set env [(Symbol var), expr] = evalExpr env expr >>= setVar env var
set _ args = malformedSyntax "set!" args

malformedSyntax :: String -> [SchemeVal] -> SchemeM a
malformedSyntax keyword args = throwError $ SyntaxError msg form
  where msg = "malformed " ++ keyword
        form = toSchemeVal $ Symbol keyword : args

-- Primitive Procedures

primitiveProcs = [("display", Proc display),
                  ("+", Proc $ toSchemeNumericOp add),
                  ("-", Proc $ toSchemeNumericOp sub),
                  ("*", Proc $ toSchemeNumericOp mul)]

display :: SchemeProc
display [val] = liftIO $ print val >> return Unspecified
display args = throwError $ ArgumentError "1" args 

type NumericOp = [Integer] -> Either SchemeError Integer

toSchemeNumericOp :: NumericOp -> SchemeProc
toSchemeNumericOp op ns = mapM fromSchemeVal ns >>=
                          liftError . liftM toSchemeVal . op

add :: NumericOp
add = return . foldl' (+) 0

sub :: NumericOp
sub [] = throwError $ ArgumentError "at least 1" []
sub [n] = return $ negate n
sub ns = return $ foldl1' (-) ns

mul :: NumericOp
mul = return . foldl' (*) 1
