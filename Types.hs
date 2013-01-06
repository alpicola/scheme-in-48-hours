{-# LANGUAGE FlexibleInstances,GeneralizedNewtypeDeriving #-}
module Types where

import Control.Monad
import Control.Monad.Error
import Control.Arrow

import Data.IORef
import Data.Map (Map)
import qualified Data.Map as Map

-- Errors

data SchemeError = Error String

instance Error SchemeError where
  strMsg = Error

instance Show SchemeError where
  show (Error msg) = "Error: " ++ msg

newtype SchemeM a = SchemeM { runSchemeM :: ErrorT SchemeError IO a }
  deriving (Monad, MonadIO, MonadError SchemeError)

liftError :: Either SchemeError a -> SchemeM a
liftError (Left e) = throwError e
liftError (Right val) = return val

-- Values

data SchemeVal = Symbol String
               | Bool Bool
               | Number Integer
               | Proc SchemeProc
               | Syntax SchemeSyntax
               | Pair SchemeVal SchemeVal
               | Nil
               | Unspecified

type SchemeProc = [SchemeVal] -> SchemeM SchemeVal
type SchemeSyntax = SchemeEnv -> [SchemeVal] -> SchemeM SchemeVal

class PrimitiveVal a where
  fromSchemeVal :: SchemeVal -> SchemeM a
  toSchemeVal :: a -> SchemeVal

instance PrimitiveVal Integer where
  fromSchemeVal (Number n) = return n
  fromSchemeVal val = throwError $ strMsg ("not number: " ++  show val)
  toSchemeVal = Number

instance PrimitiveVal [SchemeVal] where
  fromSchemeVal val = unfold val
    where unfold (Pair car cdr@(Pair _ _)) = liftM (car :) $ unfold cdr
          unfold (Pair car Nil) = return [car]
          unfold Nil = return []
          unfold _ = throwError $ strMsg ("improper list: " ++ show val)
  toSchemeVal = foldr Pair Nil

instance PrimitiveVal () where
  fromSchemeVal _ = return ()
  toSchemeVal _ = Unspecified

-- Variables

type SchemeFrame = Map String (IORef SchemeVal)
type SchemeEnv = [SchemeFrame]

refVar :: SchemeEnv -> String -> SchemeM (IORef SchemeVal)
refVar env var = maybe (throwError $ strMsg ("unbound variable: " ++ var))
                       return
                       (msum $ map (Map.lookup var) env)

getVar :: SchemeEnv -> String -> SchemeM SchemeVal
getVar env var = (refVar env var) >>= liftIO . readIORef

setVar :: SchemeEnv -> String -> SchemeVal -> SchemeM ()
setVar env var val = (refVar env var) >>= liftIO . flip writeIORef val

nullEnv :: SchemeEnv
nullEnv = []

extendEnv :: SchemeEnv -> SchemeFrame -> SchemeEnv
extendEnv = flip (:)

makeFrame :: [(String, SchemeVal)] -> SchemeM SchemeFrame
makeFrame = liftIO . liftM Map.fromList . mapM (snd newIORef)
  where snd = runKleisli . second . Kleisli

-- External representations

instance Show SchemeVal where
  show (Symbol str) = str
  show (Bool True)  = "#t"
  show (Bool False) = "#f"
  show (Number int) = show int
  show Nil          = "()"
  show _            = undefined
