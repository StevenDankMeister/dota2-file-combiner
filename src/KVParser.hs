module KVParser where

import Control.Applicative (some)
import Control.Monad (void)
import Data.Functor.Identity (Identity)
import ParserUtils (testParser)
import System.IO (IOMode (ReadMode), hGetContents, hSetEncoding, openFile, utf8)
import Text.Parsec (char, eof, errorPos, lookAhead, many, manyTill, noneOf, oneOf, option, parse, runParser, skipMany, sourceColumn, sourceLine, space, string, try, (<|>))
import Text.Parsec.Char (spaces)
import Text.Parsec.Error (ParseError, errorMessages, messageString)
import Text.Parsec.Language (LanguageDef, emptyDef)
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as P

data Root = Root String Nodes deriving (Show)
type Nodes = [Node]
data Node = KVNode KVPair | NestedRoot Root deriving (Show)

type KVPair = (String, String)
type KVFile = Root

languageDEF :: P.LanguageDef st
languageDEF =
  emptyDef
    { P.commentLine = "//"
    }

lexer :: P.GenTokenParser String u Identity
lexer = P.makeTokenParser languageDEF

parseLexeme :: String -> Parser ()
parseLexeme l = do
  P.whiteSpace lexer
  void $ string l
  P.whiteSpace lexer

parseKey :: Parser String
parseKey = do
  P.whiteSpace lexer

  void $ char '\"'
  key <- many $ noneOf "\""
  void $ char '\"'

  return key

parseKVPair :: Parser Node
parseKVPair = do
  key <- parseKey
  value <- parseKey
  return $ KVNode (key, value)

parseNestedLeaf :: Parser Node
parseNestedLeaf = NestedRoot <$> parseNestedValue

parseNodes :: Parser Nodes
parseNodes = many (try parseKVPair <|> try parseNestedLeaf)

parseNestedValue :: Parser Root
parseNestedValue = do
  key <- parseKey
  parseLexeme "{"
  nodes <- parseNodes
  parseLexeme "}"
  return $ Root key nodes

parseKVFile :: String -> IO (Either ParseError KVFile)
parseKVFile filename = do
  handle <- openFile filename ReadMode
  hSetEncoding handle utf8
  contents <- hGetContents handle
  case parse parseNestedValue "" contents of
    Left err -> do
      let pos = errorPos err
      putStrLn $ "Parse error at " ++ (show . sourceLine . errorPos $ err) ++ ":" ++ (show . sourceColumn . errorPos $ err)
      let remainingInput = dropWhile (/= '\n') $ drop (sourceColumn pos - 1) contents
      putStrLn $ "Remaining input: " ++ remainingInput
      return $ Left err
    Right val -> return $ Right val
