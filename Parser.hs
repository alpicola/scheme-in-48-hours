module Parser where

import Control.Applicative hiding ((<|>), many, optional)
import Data.Char
import Text.Parsec
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
           <|> parseSymbol
           <|> parseBool
           <|> parseList
           <|> parseQuated

readExpr :: String -> Either ParseError SchemeVal
readExpr = parse parseExpr "scheme"
