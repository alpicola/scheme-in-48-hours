module Main where

import System.Environment
import Types
import Parser

main :: IO ()
main = interact $ either show show . readExpr
