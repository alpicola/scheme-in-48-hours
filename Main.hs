module Main where

import Control.Monad.Error

import Types
import Core
import Parser

main :: IO ()
main = getContents >>= runScheme >>= putStrLn . either show show

runScheme :: String -> IO (Either SchemeError SchemeVal)
runScheme src = runErrorT . runSchemeM $ do
  env <- primitiveEnv
  liftError (readExprs src) >>= evalExprs env
