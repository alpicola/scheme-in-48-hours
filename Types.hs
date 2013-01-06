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

class Convertible a where
  fromSchemeVal :: SchemeVal -> SchemeM a
  toSchemeVal :: a -> SchemeVal

instance Convertible SchemeVal where
  fromSchemeVal = return
  toSchemeVal = id

instance Convertible Bool where
  fromSchemeVal (Bool b) = return b
  fromSchemeVal val = throwError $ strMsg ("not boolean: " ++  show val)
  toSchemeVal = Bool

instance Convertible Integer where
  fromSchemeVal (Number n) = return n
  fromSchemeVal val = throwError $ strMsg ("not number: " ++  show val)
  toSchemeVal = Number

instance Convertible a => Convertible [a] where
  fromSchemeVal p@(Pair _ _) = unfold p
    where unfold (Pair car cdr) = liftM2 (:) (fromSchemeVal car) (unfold cdr)
          unfold Nil = return []
          unfold _ = throwError $ strMsg ("improper list: " ++ show p)
  fromSchemeVal val = throwError $ strMsg ("not list: " ++ show val)
  toSchemeVal = foldr Pair Nil . map toSchemeVal

-- Variables

type SchemeVar = IORef SchemeVal
type SchemeFrame = Map String SchemeVar
type SchemeEnv = [SchemeFrame]

refVar :: SchemeEnv -> String -> SchemeM SchemeVar
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
  show (Symbol s)   = s
  show (Bool True)  = "#t"
  show (Bool False) = "#f"
  show (Number n)   = show n
  show (Proc _)     = "#<procedure>"
  show (Syntax _)   = "#<syntax>"
  show p@(Pair _ _) = "(" ++ showList p ++ ")"
    where showList (Pair car Nil) = show car
          showList (Pair car cdr) = show car ++ " " ++ showList cdr 
          showList Nil = ""
          showList val = " . " ++ show val 
  show Nil          = "()"
  show Unspecified  = "#<unspecified>"
