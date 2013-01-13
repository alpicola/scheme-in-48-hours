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
parseList = parens $ flip (foldr Pair) <$> parseDatums <*> option Nil dotted
  where dotted = char '.' *> parseDatum
        parens = between (char '(') (char ')')

parseQuated :: Parser SchemeVal
parseQuated = quote <$> (char '\'' *> parseDatum)
  where quote = Pair (Symbol "quote") . flip Pair Nil

parseDatum :: Parser SchemeVal
parseDatum = spaces *> form
  where form = try parseNumber
           <|> try parseSymbol
           <|> parseBool
           <|> parseList
           <|> parseQuated

parseDatums :: Parser [SchemeVal]
parseDatums = sepEndBy parseDatum spaces

runParser :: Parser a -> String -> Either SchemeError a
runParser parser input = either (throwError . ParseError) return
                                (parse parser "scheme" input)

readDatum :: String -> Either SchemeError SchemeVal
readDatum = runParser parseDatum

readDatums :: String -> Either SchemeError [SchemeVal]
readDatums =  runParser parseDatums
