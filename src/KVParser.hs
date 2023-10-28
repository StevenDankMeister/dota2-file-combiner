module KVParser where

import Control.Applicative (some)
import Control.Monad (void)
import Data.Functor.Identity (Identity)
import Data.List (intercalate)
import ParserUtils
import System.IO (IOMode (ReadMode), hGetContents, hSetEncoding, openFile, utf8)
import Text.Parsec (char, eof, errorPos, lookAhead, many, manyTill, noneOf, oneOf, parse, runParser, skipMany, sourceColumn, sourceLine, space, try, (<|>))
import Text.Parsec.Char (spaces)
import Text.Parsec.Error (ParseError, errorMessages, messageString)
import Text.Parsec.Language (LanguageDef, emptyDef)
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as P

newtype Key = Key String deriving (Show)
data Value = Value [KVPair] Value | ListValue Key Value | End deriving (Show)

type KVPair = (String, String)
type KVFile = Value

languageDEF :: P.LanguageDef st
languageDEF =
  emptyDef
    { P.commentLine = "//"
    }

lexer :: P.GenTokenParser String u Identity
lexer = P.makeTokenParser languageDEF

parseKey :: Parser String
parseKey = do
  void $ char '\"'
  key <- many $ noneOf "\""
  void $ char '\"'
  return key

parseValue :: Parser Value
parseValue = try parseEnd <|> try parseKVPairs <|> try parseListValue

parseKVPair :: Parser KVPair
parseKVPair = do
  key <- parseKey
  void spaces
  value <- parseKey
  return (key, value)

parseKVPairs :: Parser Value
parseKVPairs = do
  kvpairs <- manyTill (parseKVPair <* P.whiteSpace lexer) (lookAhead $ char '{' <|> char '}')
  Value kvpairs <$> parseValue

parseListValue :: Parser Value
parseListValue = do
  P.whiteSpace lexer
  key <- Key <$> parseKey
  P.whiteSpace lexer
  void $ char '{'
  value <- parseValue
  void $ char '}'
  return $ ListValue key value

parseEnd :: Parser Value
parseEnd = do
  eof
  return End

parseKVFile :: String -> IO (Either ParseError Value)
parseKVFile filename = do
  handle <- openFile filename ReadMode
  hSetEncoding handle utf8
  contents <- hGetContents handle
  case parse parseListValue "" contents of
    Left err -> do
      let pos = errorPos err
      putStrLn $ "Parse error at " ++ (show . sourceLine . errorPos $ err) ++ ":" ++ (show . sourceColumn . errorPos $ err)
      let remainingInput = dropWhile (/= '\n') $ drop (sourceColumn pos - 1) contents
      putStrLn $ "Remaining input: " ++ remainingInput
      return $ Left err
    Right val -> return $ Right val
