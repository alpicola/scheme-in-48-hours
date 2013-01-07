module Parser where

import Control.Monad.Error
import Control.Applicative hiding ((<|>), many, optional)

import Data.Char
import Text.Parsec hiding (runParser)
import Text.Parsec.String

import Types

parseSymbol :: Parser SchemeVal
parseSymbol = Symbol <$> symbol
  where start  = letter <|> oneOf "!$%*/:<=>?@^~"
        symbol = (:) <$> start <*> many (start <|> digit <|> oneOf "+-.")
             <|> string "+" <|> string "-" <|> string "..."

parseBool :: Parser SchemeVal
parseBool = Bool . ('t' ==) <$> (char '#' *> oneOf "tf")

parseNumber :: Parser SchemeVal
parseNumber = Number . read <$> signed 
  where signed = optional (char '+') *> many1 digit
             <|> (:) <$> char '-' <*> many1 digit

parseList :: Parser SchemeVal
parseList = parens $ (flip $ foldr Pair) <$> exprs <*> option Nil dotted
  where exprs  = sepEndBy parseExpr spaces
        dotted = char '.' *> parseExpr
        parens = between (char '(') (char ')')

parseQuated :: Parser SchemeVal
parseQuated = quote <$> (char '\'' *> parseExpr)
  where quote = Pair (Symbol "quote") . flip Pair Nil

parseExpr :: Parser SchemeVal
parseExpr = spaces *> expr
  where expr = try parseNumber
           <|> try parseSymbol
           <|> parseBool
           <|> parseList
           <|> parseQuated

runParser :: Parser a -> String -> Either SchemeError a
runParser parser input = case parse parser "scheme" input of
                           Left error -> throwError $ ParseError error 
                           Right val -> return val

readExpr :: String -> Either SchemeError SchemeVal
readExpr = runParser parseExpr 

readExprs :: String -> Either SchemeError [SchemeVal]
readExprs =  runParser $ sepEndBy parseExpr spaces
