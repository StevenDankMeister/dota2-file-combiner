module ParserUtils where

import Control.Monad (void)
import Text.Parsec (ParseError, anyChar, char, choice, digit, many, many1, manyTill, newline, noneOf, oneOf, optionMaybe, parse, runParser, sepEndBy1, skipMany, spaces, string, try)
import Text.Parsec.Combinator (option)
import Text.Parsec.String (Parser)
import Text.Parsec.Token (GenTokenParser (whiteSpace))

parseInt :: Parser Int
parseInt = do
  sign <- option ' ' $ char '-'
  num <- many1 digit
  return $ read (sign : num)

parseListBySep :: Parser a -> Char -> Parser [a]
parseListBySep p sep = sepEndBy1 p (char sep)

parseListInt :: Parser [Int]
parseListInt = parseListBySep parseInt ' '

testParser :: Parser a -> String -> Either ParseError a
testParser p = parse p ""
