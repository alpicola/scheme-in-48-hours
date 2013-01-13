module Core (runScheme) where

import Control.Arrow
import Control.Monad
import Control.Monad.Error

import Data.IORef
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map

import Types
import Parser
import Macro hiding (extendEnv, makeFrame)

runScheme :: String -> IO (Either SchemeError SchemeVal)
runScheme src = runErrorT . runSchemeM $ do
    forms <- liftError $ readDatums src
    env <- primitiveEnv
    expandTopLevel (toMacroEnv env) forms >>= evalExpr env

-- Environment

type SchemeVar = IORef SchemeVal
type SchemeFrame = Map String SchemeVar
type SchemeEnv = [SchemeFrame]

nullEnv :: SchemeEnv
nullEnv = []

extendEnv :: SchemeEnv -> SchemeFrame -> SchemeEnv
extendEnv = flip (:)

makeFrame :: [(String, SchemeVal)] -> SchemeM SchemeFrame
makeFrame = liftIO . liftM Map.fromList . mapM (second' newIORef)
  where second' = runKleisli . second . Kleisli

refVar :: SchemeEnv -> String -> SchemeM SchemeVar
refVar env var = maybe (throwError $ Error ("unbound variable: " ++ var))
                       return (msum $ map (Map.lookup var) env)

getVar :: SchemeEnv -> String -> SchemeM SchemeVal
getVar env var = refVar env var >>= liftIO . readIORef

setVar :: SchemeEnv -> String -> SchemeVal -> SchemeM SchemeVal
setVar env var val = refVar env var >>= liftIO . flip writeIORef val >>
                     return Unspecified

toMacroEnv  :: SchemeEnv -> MacroEnv
toMacroEnv = map $ Map.map (const Nothing)

-- Evalution

evalExpr :: SchemeEnv -> SchemeForm -> SchemeM SchemeVal
evalExpr env (Val val) = return val 
evalExpr env (Var var) = getVar env var
evalExpr env (App expr args) = do proc <- evalExpr env expr >>= fromSchemeVal
                                  mapM (evalExpr env) args >>= proc
evalExpr env (Lambda vars dotted body) = return . Proc $ \args -> do
    bindings <- bind vars dotted args
    env <- extendEnv env `liftM` makeFrame bindings
    evalExprs env body
  where
    bind vars dotted args = bind' vars args
      where 
        num = show $ length vars
        bind' (var : vars) (val : vals) = liftM ((var, val) :) $ bind' vars vals
        bind' (_:_) [] = throwError $ ArgumentError num args
        bind' [] [] = return []
        bind' [] vals = maybe (throwError $ ArgumentError num args)
                              (\var -> return [(var, toSchemeVal vals)]) dotted
evalExpr env (If test expr expr') = do test <- evalExpr env test >>= fromSchemeVal
                                       evalExpr env (if test then expr else expr')
evalExpr env (Set var expr) = evalExpr env expr >>= setVar env var

evalExprs :: SchemeEnv -> [SchemeForm] -> SchemeM SchemeVal
evalExprs env [] = throwError $ Error "empty body"
evalExprs env [expr] = evalExpr env expr
evalExprs env (expr : exprs) = evalExpr env expr >> evalExprs env exprs

-- Primitives

primitiveEnv :: SchemeM SchemeEnv
primitiveEnv = extendEnv nullEnv `liftM` makeFrame primitiveProcs

primitiveProcs = [ ("display", Proc display)
                 , ("+", Proc $ toSchemeProc add)
                 , ("-", Proc $ toSchemeProc sub)
                 , ("*", Proc $ toSchemeProc mul)
                 , ("<", Proc $ toSchemeProc lt)
                 , (">", Proc $ toSchemeProc gt)]

toSchemeProc :: (Convertible a, Convertible b) =>
                    ([a] -> Either SchemeError b) -> SchemeProc
toSchemeProc f args = mapM fromSchemeVal args >>=
                      liftError . liftM toSchemeVal . f

display [val] = liftIO $ print val >> return Unspecified
display args = throwError $ ArgumentError "1" args 

pairs :: [a] -> [(a, a)]
pairs (x:x':xs) = (x, x') : pairs (x':xs)
pairs _ = []

add :: [Integer] -> Either SchemeError Integer
add = return . foldl' (+) 0

sub :: [Integer] -> Either SchemeError Integer
sub [] = throwError $ ArgumentError "at least 1" []
sub [n] = return $ negate n
sub ns = return $ foldl1' (-) ns

mul :: [Integer] -> Either SchemeError Integer
mul = return . foldl' (*) 1

lt :: [Integer] -> Either SchemeError Bool
lt ns@(_:_:_) = return . all (uncurry (<)) $ pairs ns
lt args = throwError $ ArgumentError "at least 2" (map toSchemeVal args)

gt :: [Integer] -> Either SchemeError Bool
gt ns@(_:_:_) = return . all (uncurry (>)) $ pairs ns
gt args = throwError $ ArgumentError "at least 2" (map toSchemeVal args)
