module Main where

import Control.Monad

import System.Environment

import Types
import Core

main :: IO ()
main = do args <- getArgs
          src <- if null args
                     then getContents
                     else liftM concat $ mapM readFile args 
          runScheme src >>= either (putStrLn . show) (const $ return ())
