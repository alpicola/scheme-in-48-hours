module Types where

import Data.IORef
import Data.Map (Map)
import qualified Data.Map as Map

data SchemeVal = Symbol String
               | Bool Bool
               | Number Integer
               | Proc [SchemeVal] [SchemeVal] Env
               -- | PrimProc ([SchemeVal] -> SchemeVal)
               | Pair SchemeVal SchemeVal
               | Nil
               deriving Show

toSchemeList :: [SchemeVal] -> SchemeVal
toSchemeList = foldr Pair Nil

type Env = [Frame]
type Frame = Map String SchemeVal
