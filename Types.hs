{-# LANGUAGE FlexibleInstances,GeneralizedNewtypeDeriving #-}
module Types where

import Control.Monad
import Control.Monad.Error
import Control.Arrow

import Data.IORef
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map

import Text.Parsec (ParseError)

-- Errors

data SchemeError = Error String
                 | TypeError String SchemeVal
                 | ArgumentError String [SchemeVal]
                 | SyntaxError String SchemeVal
                 | ParseError ParseError

instance Error SchemeError where
  strMsg = Error

instance Show SchemeError where
  show = showError

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

instance Show SchemeVal where
  show = showVal

class Convertible a where
  fromSchemeVal :: SchemeVal -> SchemeM a
  toSchemeVal :: a -> SchemeVal

instance Convertible SchemeVal where
  fromSchemeVal = return
  toSchemeVal = id

instance Convertible String where
  fromSchemeVal (Symbol s) = return s
  fromSchemeVal val = throwError $ TypeError "symbol" val
  toSchemeVal = Symbol

instance Convertible Bool where
  fromSchemeVal (Bool b) = return b
  fromSchemeVal val = throwError $ TypeError "boolean" val
  toSchemeVal = Bool

instance Convertible Integer where
  fromSchemeVal (Number n) = return n
  fromSchemeVal val = throwError $ TypeError "number" val
  toSchemeVal = Number

instance Convertible [SchemeVal] where
  fromSchemeVal p@(Pair _ _) = unfold p
    where unfold (Pair car cdr) = liftM (car :) $ unfold cdr
          unfold Nil = return []
          unfold _ = throwError $ TypeError "proper list" p
  fromSchemeVal Nil = return []
  fromSchemeVal val = throwError $ TypeError "list" val
  toSchemeVal = foldr Pair Nil

isSymbol :: SchemeVal -> Bool
isSymbol (Symbol _) = True
isSymbol _ = False

isBool :: SchemeVal -> Bool
isBool (Bool _) = True
isBool _ = False

isNumber :: SchemeVal -> Bool
isNumber (Number _) = True
isNumber _ = False

isProc :: SchemeVal -> Bool
isProc (Proc _) = True
isProc _ = False

isPair :: SchemeVal -> Bool
isPair (Pair _ _) = True
isPair _ = False

isNil :: SchemeVal -> Bool
isNil Nil = True
isNil _ = False

-- Variables

type SchemeVar = IORef SchemeVal
type SchemeFrame = Map String SchemeVar
type SchemeEnv = [SchemeFrame]

refVar :: SchemeEnv -> String -> SchemeM SchemeVar
refVar env var = maybe (throwError $ Error ("unbound variable: " ++ var))
                       return
                       (msum $ map (Map.lookup var) env)

getVar :: SchemeEnv -> String -> SchemeM SchemeVal
getVar env var = (refVar env var) >>= liftIO . readIORef

setVar :: SchemeEnv -> String -> SchemeVal -> SchemeM SchemeVal
setVar env var val = (refVar env var) >>= liftIO . flip writeIORef val >>
                     return Unspecified

nullEnv :: SchemeEnv
nullEnv = []

extendEnv :: SchemeEnv -> SchemeFrame -> SchemeEnv
extendEnv = flip (:)

makeFrame :: [(String, SchemeVal)] -> SchemeM SchemeFrame
makeFrame = liftIO . liftM Map.fromList . mapM (snd newIORef)
  where snd = runKleisli . second . Kleisli

-- External representations

showError :: SchemeError -> String
showError (Error msg) = "Error: " ++ msg
showError (TypeError expected found) = "Type eror: expected " ++ expected ++
                                       ", found " ++ show found
showError (ArgumentError num []) = "Argument error: required " ++ num
showError (ArgumentError num found) = "Argument error: expected " ++ num ++
                                      ", found " ++ show (length found) ++
                                      ": " ++ intercalate ", " (map show found)
showError (SyntaxError msg form) = "Syntax error: " ++ msg ++
                                   ": " ++ show form
showError (ParseError error) = "Parse error: " ++ show error

showVal :: SchemeVal -> String
showVal (Symbol s) = s
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (Number n) = show n
showVal (Proc _) = "#<procedure>"
showVal (Syntax _) = "#<syntax>"
showVal pair@(Pair _ _) = "(" ++ show' pair ++ ")"
  where show' (Pair car Nil) = showVal car
        show' (Pair car cdr) = showVal car ++ " " ++ show' cdr 
        show' Nil = ""
        show' val = " . " ++ showVal val 
showVal Nil = "()"
showVal Unspecified = "#<unspecified>"
