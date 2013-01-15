module Core (runScheme) where

import Control.Applicative
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
    form <- liftError $ expandTopLevel (toMacroEnv env) forms
    evalExpr env form

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
evalExpr env (App expr args) = do
    proc <- evalExpr env expr >>= liftError . fromSchemeVal
    mapM (evalExpr env) args >>= proc
evalExpr env (Lambda vars dotted body) = return . Proc $ \args -> do
    bindings <- bind vars dotted args
    env <- extendEnv env <$> makeFrame bindings
    evalExpr env body
  where
    bind vars dotted args = bind' vars args
      where 
        num = show $ length vars
        bind' (var : vars) (val : vals) = ((var, val) :) <$> bind' vars vals
        bind' (_:_) [] = throwError $ ArgumentError num args
        bind' [] [] = return []
        bind' [] vals = maybe (throwError $ ArgumentError num args)
                              (\var -> return [(var, toSchemeVal vals)]) dotted
evalExpr env (If test expr expr') = do
    test <- evalExpr env test >>= liftError . fromSchemeVal
    evalExpr env (if test then expr else expr')
evalExpr env (Set var expr) = evalExpr env expr >>= setVar env var
evalExpr env (Begin exprs) = foldM (const $ evalExpr env) Unspecified exprs

-- Procedures

primitiveEnv :: SchemeM SchemeEnv
primitiveEnv = extendEnv nullEnv <$> makeFrame (map (second Proc) primitiveProcs)

primitiveProcs :: [(String, SchemeProc)]
primitiveProcs = [ ("boolean?",   oneArg $ return . Bool . isBool)
                 , ("pair?",      oneArg $ return . Bool . isPair)
                 , ("symbole?",   oneArg $ return . Bool . isSymbol)
                 , ("number?",    oneArg $ return . Bool . isNumber)
                 , ("procedure?", oneArg $ return . Bool . isProc)
                 , ("null?",      oneArg $ return . Bool . isNil)
                 , ("equal?",     twoArg $ ((return . Bool) .) . (==))
                 , ("=",  toSchemeProc $ allPairs (==))
                 , ("<",  toSchemeProc $ allPairs (<))
                 , (">",  toSchemeProc $ allPairs (>))
                 , ("<=", toSchemeProc $ allPairs (<=))
                 , (">=", toSchemeProc $ allPairs (>=))
                 , ("+",  toSchemeProc add)
                 , ("-",  toSchemeProc sub)
                 , ("*",  toSchemeProc mul)
                 , ("cons",    cons)
                 , ("car",     car)
                 , ("cdr",     cdr)
                 , ("apply",   apply)
                 , ("display", display) ]

toSchemeProc :: (Convertible a, Convertible b) =>
                    ([a] -> Either SchemeError b) -> SchemeProc
toSchemeProc f args = liftError . liftM toSchemeVal $ mapM fromSchemeVal args >>= f

oneArg :: (SchemeVal -> SchemeM SchemeVal) -> SchemeProc
oneArg f [arg] = f arg
oneArg f args  = throwError $ ArgumentError "1" args

twoArg :: (SchemeVal -> SchemeVal -> SchemeM SchemeVal) -> SchemeProc
twoArg f [arg1, arg2] = f arg1 arg2
twoArg f args  = throwError $ ArgumentError "2" args

allPairs :: (Integer -> Integer -> Bool) -> [Integer] -> Either SchemeError Bool
allPairs op args@(_:_:_) = return . all (uncurry op) $ pairs args
  where pairs (x:y:xs) = (x, y) : pairs (y:xs)
        pairs _ = []
allPairs op args = throwError $ ArgumentError "at least 2" (map toSchemeVal args)

add :: [Integer] -> Either SchemeError Integer
add = return . foldl' (+) 0

sub :: [Integer] -> Either SchemeError Integer
sub [] = throwError $ ArgumentError "at least 1" []
sub [n] = return $ negate n
sub ns = return $ foldl1' (-) ns

mul :: [Integer] -> Either SchemeError Integer
mul = return . foldl' (*) 1

cons :: SchemeProc
cons = twoArg $ (return .) . Pair

car :: SchemeProc
car = oneArg car'
  where car' (Pair val _) = return val
        car' val = throwError $ TypeError "pair" val

cdr :: SchemeProc
cdr = oneArg cdr'
  where cdr' (Pair _ val) = return val
        cdr' val = throwError $ TypeError "pair" val

apply :: SchemeProc
apply (val : args@(_:_)) = do
    proc <- liftError $ fromSchemeVal val
    liftError (fromSchemeVal (last args)) >>= proc . (init args ++)
apply args = throwError $ ArgumentError "at least 2" args

display :: SchemeProc
display = oneArg $ \val -> liftIO $ print val >> return Unspecified
