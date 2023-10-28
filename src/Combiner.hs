module Combiner where

import KVParser

import Data.Char (toUpper)
import System.Directory (canonicalizePath, getDirectoryContents, listDirectory)
import System.FilePath
import Text.Parsec.Error (ParseError)

folderNames :: [String]
folderNames = ["abilities", "heroes", "items", "units"]

firstKeys :: [String]
firstKeys = map setFirstKey folderNames

setFirstKey :: [Char] -> [Char]
setFirstKey (x : xs) = "DOTA" ++ toUpper x : xs
setFirstKey [] = ""

customNames :: [String]
customNames = map (\x -> "npc_" ++ x ++ "_custom.txt") folderNames

appendKVFile :: FilePath -> Either ParseError KVFile -> IO ()
appendKVFile combinedFileName (Right kvFile) = do
  let content = show kvFile
  appendFile combinedFileName content
appendKVFile _ (Left err) = error $ show err

clearOrCreateFile :: FilePath -> IO ()
clearOrCreateFile filename = writeFile filename ""

-- Initial nesting to 1 since the combined
-- files always are nested by 1
parseAllFilesInFolder :: [FilePath] -> IO [Either ParseError KVFile]
parseAllFilesInFolder files = sequence [parseKVFile file 1 | file <- files]

combineFromFolder :: [FilePath] -> FilePath -> IO ()
combineFromFolder kvFiles targetFile = do
  print kvFiles
  parsedFiles <- parseAllFilesInFolder kvFiles
  clearOrCreateFile targetFile
  mapM_ (appendKVFile targetFile) parsedFiles

appendPathToFiles :: FilePath -> [FilePath] -> [FilePath]
appendPathToFiles path = map (path </>)

combine :: FilePath -> IO ()
combine pathToNpc = do
  abosluteToNpc <- canonicalizePath pathToNpc

  let pathsToKVsFolders = map (abosluteToNpc </>) folderNames
  kvsFromDirectory <- mapM listDirectory pathsToKVsFolders

  -- This is unreadable
  let kvsWithPath = zipWith appendPathToFiles pathsToKVsFolders kvsFromDirectory

  -- The _custom files should go to the npc folder
  let customNamesInNpc = map (abosluteToNpc </>) customNames

  -- This is also unreadable
  mapM_ (uncurry combineFromFolder) $ zip kvsWithPath customNamesInNpc
