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

data Root = Root String Nodes NestingLevel

instance Show Root where
  show (Root key elems nestingLevel) =
    whitespaces nestingLevel
      ++ show key
      ++ "\n"
      ++ whitespaces nestingLevel
      ++ "{\n"
      ++ show elems
      ++ "\n"
      ++ whitespaces nestingLevel
      ++ "}\n"

data Nodes = Nodes [Node] NestingLevel

instance Show Nodes where
  show (Nodes (x : xs) nestingLevel) =
    show x ++ show (Nodes xs nestingLevel)
  show (Nodes [] _) = ""

data Node = KVNode KVPair NestingLevel | NestedRoot Root NestingLevel

instance Show Node where
  show (KVNode (x, y) nestingLevel) =
    whitespaces nestingLevel ++ show x ++ "    " ++ show y ++ "\n"
  show (NestedRoot root _) = show root

type KVPair = (String, String)
type KVFile = Root
type NestingLevel = Int

whitespaces :: NestingLevel -> String
whitespaces n = replicate (n * 2) ' '

doubleQuotes :: String
doubleQuotes = "\""

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

parseKVPair :: NestingLevel -> Parser Node
parseKVPair nestingLevel = do
  key <- parseKey
  value <- parseKey
  return $ KVNode (key, value) nestingLevel

parseNestedRoot :: NestingLevel -> Parser Node
parseNestedRoot nestingLevel = do
  nested <- parseNestedValue nestingLevel
  return $ NestedRoot nested nestingLevel

parseNodes :: NestingLevel -> Parser Nodes
parseNodes nestingLevel = do
  nodes <- many (try (parseKVPair nestingLevel) <|> try (parseNestedRoot nestingLevel))
  return $ Nodes nodes nestingLevel

parseNestedValue :: NestingLevel -> Parser Root
parseNestedValue nestingLevel = do
  key <- parseKey
  parseLexeme "{"
  nodes <- parseNodes $ nestingLevel + 1
  parseLexeme "}"
  return $ Root key nodes nestingLevel

parseKVFile :: String -> NestingLevel -> IO (Either ParseError KVFile)
parseKVFile filename initialNesting = do
  handle <- openFile filename ReadMode
  hSetEncoding handle utf8
  contents <- hGetContents handle
  case parse (parseNestedValue initialNesting) "" contents of
    Left err -> do
      let pos = errorPos err
      putStrLn $ "Parse error at " ++ (show . sourceLine . errorPos $ err) ++ ":" ++ (show . sourceColumn . errorPos $ err)
      let remainingInput = dropWhile (/= '\n') $ drop (sourceColumn pos - 1) contents
      putStrLn $ "Remaining input: " ++ remainingInput
      return $ Left err
    Right val -> return $ Right val
