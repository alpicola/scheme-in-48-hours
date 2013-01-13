{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving #-}
module Types where

import Control.Monad
import Control.Monad.Error

import Data.List

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
  show (Error msg) = "Error: " ++ msg
  show (TypeError expected found) = "Type eror: expected " ++ expected ++
                                    ", found " ++ show found
  show (ArgumentError num []) = "Argument error: required " ++ num
  show (ArgumentError num found) = "Argument error: expected " ++ num ++
                                   ", found " ++ show (length found) ++
                                   ": " ++ intercalate ", " (map show found)
  show (SyntaxError msg form) = "Syntax error: " ++ msg ++
                                ": " ++ show form
  show (ParseError err) = "Parse error: " ++ show err

newtype SchemeM a = SchemeM { runSchemeM :: ErrorT SchemeError IO a }
  deriving (Monad, MonadIO, MonadError SchemeError)

liftError :: MonadError e m => Either e a -> m a
liftError = either throwError return

-- Values

data SchemeVal = Bool Bool
               | Pair SchemeVal SchemeVal
               | Symbol String
               | Number Integer
               | Proc SchemeProc
               | Nil
               | Unspecified
               | Undefined

type SchemeProc = [SchemeVal] -> SchemeM SchemeVal

instance Show SchemeVal where
  show (Bool True) = "#t"
  show (Bool False) = "#f"
  show pair@(Pair _ _) = "(" ++ show' pair ++ ")"
    where show' (Pair car Nil) = show car
          show' (Pair car cdr) = show car ++ " " ++ show' cdr 
          show' Nil = ""
          show' val = " . " ++ show val 
  show (Symbol s) = s
  show (Number n) = show n
  show (Proc _) = "#<procedure>"
  show Nil = "()"
  show Unspecified = "#<unspecified>"
  show Undefined = "#<undefined>"

class Convertible a where
  fromSchemeVal :: SchemeVal -> SchemeM a
  toSchemeVal :: a -> SchemeVal

instance Convertible SchemeVal where
  fromSchemeVal = return
  toSchemeVal = id

instance Convertible Bool where
  fromSchemeVal (Bool b) = return b
  fromSchemeVal val = throwError $ TypeError "boolean" val
  toSchemeVal = Bool

instance Convertible [SchemeVal] where
  fromSchemeVal p@(Pair _ _) = unfold p
    where unfold (Pair car cdr) = liftM (car :) $ unfold cdr
          unfold Nil = return []
          unfold _ = throwError $ TypeError "proper list" p
  fromSchemeVal Nil = return []
  fromSchemeVal val = throwError $ TypeError "list" val
  toSchemeVal = foldr Pair Nil

instance Convertible String where
  fromSchemeVal (Symbol s) = return s
  fromSchemeVal val = throwError $ TypeError "symbol" val
  toSchemeVal = Symbol

instance Convertible Integer where
  fromSchemeVal (Number n) = return n
  fromSchemeVal val = throwError $ TypeError "number" val
  toSchemeVal = Number

instance Convertible SchemeProc where
  fromSchemeVal (Proc proc) = return proc
  fromSchemeVal val = throwError $ TypeError "procedure" val
  toSchemeVal = Proc

isBool :: SchemeVal -> Bool
isBool (Bool _) = True
isBool _ = False

isPair :: SchemeVal -> Bool
isPair (Pair _ _) = True
isPair _ = False

isSymbol :: SchemeVal -> Bool
isSymbol (Symbol _) = True
isSymbol _ = False

isNumber :: SchemeVal -> Bool
isNumber (Number _) = True
isNumber _ = False

isProc :: SchemeVal -> Bool
isProc (Proc _) = True
isProc _ = False

isNil :: SchemeVal -> Bool
isNil Nil = True
isNil _ = False

-- Core Forms

data SchemeForm = Val SchemeVal
                | Var String
                | App SchemeForm [SchemeForm]
                | Lambda [String] (Maybe String) [SchemeForm]
                | If SchemeForm SchemeForm SchemeForm
                | Set String SchemeForm

instance Show SchemeForm where
  show (Val val) = show val 
  show (Var var) = var
  show (App proc args) = "(" ++ intercalate " " (map show (proc : args)) ++ ")"
  show (Lambda vars dotted body) = "(lambda (" ++ intercalate " " vars ++
                                   maybe "" (" . " ++) dotted ++ ") " ++
                                   intercalate " " (map show body) ++ ")"
  show (If test expr expr') = "(if " ++ show test ++ " " ++ show expr ++
                              " " ++ show expr' ++ ")"
  show (Set var expr) = "(set! " ++ var ++ " " ++ show expr ++ ")"
