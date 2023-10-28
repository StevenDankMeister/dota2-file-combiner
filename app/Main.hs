module Main (main) where

import KVParser
import ParserUtils
import Text.Parsec (ParseError)

main :: IO (Either ParseError KVFile)
main = parseKVFile "testfile2.txt" 1

--
-- main2 :: IO (Either ParseError KVFile)
-- main2 = parseKVFile "testfile.txt"
