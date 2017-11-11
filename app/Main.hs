{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE MultiWayIf, OverloadedStrings #-}
module Main where

import Data.Text (pack)
import Control.Logging as L
import Data.List
import Data.Time.Clock.POSIX
import Data.Time.Format
import Data.ByteString as BS (ByteString, readFile)
import Data.Maybe
import System.Directory (   setCurrentDirectory
                          , getCurrentDirectory
                          , removeFile
                          , listDirectory
                          , makeAbsolute)
import System.FilePath
import System.IO
import Crypto.Hash
import Control.Monad
import qualified System.PosixCompat.Files as PF
import Text.Printf

--import Debug.Trace

trace :: t1 -> t -> t
trace _ x = x

-- Configurations

zouFile :: FilePath
zouFile = ".zou"

zouRunFile :: FilePath
zouRunFile = ".zou.running"

encoding :: TextEncoding
encoding = utf8

fileSizeDigits :: Int
fileSizeDigits = 11 -- Enough for ~93GB max file size

timeFormat :: String
timeFormat = "%0Y-%m-%d-%H-%M-%S"

logTimeFormat :: String
logTimeFormat = "%0Y-%m-%d-%H-%M-%S%q" 

hashFunction :: ByteString -> String
hashFunction bs = show (hash bs :: Digest SHA1)

hashSize :: Int
hashSize = 160

nullHash :: String
nullHash = replicate (2 * hashSize `quot` 8) '0'
  

-- Data Types

data FileType = File | Directory | SymbolicLink | Other

isFile :: FileType -> Bool
isFile File = True
isFile _    = False

isDirectory :: FileType -> Bool
isDirectory Directory = True
isDirectory _         = False

fileTypeToString :: FileType -> String
fileTypeToString File         = "F"
fileTypeToString Directory    = "D"
fileTypeToString SymbolicLink = "L"
fileTypeToString Other        = "O"

fileTypeFromString :: String -> FileType
fileTypeFromString "F" = File
fileTypeFromString "D" = Directory
fileTypeFromString "L" = SymbolicLink
fileTypeFromString "O" = Other
fileTypeFromString _   = error "should not be here"


data FileStatus = Removed | Updated | Created deriving Show


data FileDescriptor = FileDescriptor {
  fdType :: FileType,
  fdHash :: String,
  fdSize :: String,
  fdDate :: String,
  fdName :: String
  }

storeFDToString :: FileDescriptor -> String
storeFDToString fd =
  unwords $ map (\x -> x fd) [fileTypeToString . fdType, fdHash, fdSize, fdDate, normalise . fdName]

loadFDFromString :: String -> FileDescriptor
loadFDFromString str =
     FileDescriptor (fileTypeFromString ftype) fhash fsize fdate fname
  where
     brks = words str
     [ftype, fhash, fsize, fdate] = take 4 brks
     fname = unwords $ drop 4 brks

-- Misc

hashFile :: FilePath -> IO String
hashFile fp = do
  bs <- BS.readFile fp
  return $ hashFunction bs

-- File report generation

getFileType :: PF.FileStatus -> FileType
getFileType fs =
  if | PF.isRegularFile fs  -> File
     | PF.isDirectory fs    -> Directory
     | PF.isSymbolicLink fs -> SymbolicLink
     | otherwise            -> Other
  
getFileHash :: FileType -> FilePath -> IO String
getFileHash File fp = hashFile fp
getFileHash _    _  = return nullHash

getModificationTime :: PF.FileStatus -> String
getModificationTime fs =
  formatTime defaultTimeLocale timeFormat utcTime
  where
    utcTime = posixSecondsToUTCTime $ realToFrac (PF.modificationTime fs)

generateReport :: FilePath -> FilePath -> IO ()
generateReport outputFile baseDir = do
  debug $ pack $ "Generating report for: " ++ baseDir
  
  files <- map (baseDir </>) <$> (sort <$> listDirectory baseDir)
  fileStats <- mapM PF.getSymbolicLinkStatus files
  
  -- File types
  let ftypes0 = map getFileType fileStats
  -- File hashes
  fhashes <- zipWithM getFileHash ftypes0 files
  -- File sizes
  let fsizes = map (printf ("%0" ++ show fileSizeDigits ++ "d"). toInteger . PF.fileSize) fileStats
  -- File dates
  let fmodifs = map getModificationTime fileStats

  let fall = map storeFDToString (zipWith5 FileDescriptor ftypes0 fhashes fsizes fmodifs files)
  
  -- Write file
  fhandle <- openFile outputFile AppendMode 
  hSetEncoding fhandle encoding
  _ <- mapM (hPutStrLn fhandle) fall
  hClose fhandle

  -- Recurse
  dirs <- zipWithM (\f t -> if isDirectory t then return (Just f) else return Nothing) files ftypes0
  mapM_ (generateReport outputFile) (catMaybes dirs)


-- Works on the current directory
initialize :: IO Bool
initialize = do
  fe <- PF.fileExist zouFile
  if fe then do
      L.log "File database exists, generating difference report"
      fe2 <- PF.fileExist zouRunFile
      when fe2 $ removeFile zouRunFile
      generateReport zouRunFile "."
      return True
    else do
      warn "No database found. Creating a new one."
      generateReport zouFile "."
      return False

diffLines :: [String] -> [String] -> [(FileDescriptor, FileStatus)] -> [(FileDescriptor, FileStatus)]
diffLines [] [] acc = acc
diffLines [] ys acc = trace "diffLines 1" ([(loadFDFromString y, Created) | y <- ys] ++ acc)
diffLines xs [] acc = trace "diffLines 2" ([(loadFDFromString x, Removed) | x <- xs] ++ acc)
diffLines tx@(x:xs) ty@(y:ys) acc
     | x == y =
       trace ("Difflines 3 " ++ x) (
       diffLines xs ys acc) -- Nothing to do
     | fdName fdx == fdName fdy = --File was modified
         trace ("difflines 4 " ++  fdName fdx ++  fdName fdy) (
         if fdDate fdy < fdDate fdx then error "Should not happen"
         else diffLines xs ys ((fdy, Updated):acc))
     | fdName fdx > fdName fdy = -- File created
       trace ("Difflines 4 " ++ x ++ "     " ++ y) (
         diffLines tx ys ((fdy, Created):acc)  )
     | otherwise = -- File removed
       trace ("Difflines 5 " ++ x ++ "     " ++ y) (
         diffLines xs ty ((fdx, Removed):acc))
  where
     fdx = loadFDFromString x
     fdy = loadFDFromString y


main :: IO ()
main = withStdoutLogging $ do

  setLogTimeFormat logTimeFormat
  setLogLevel LevelInfo
  
  let rootDir = "/home/emilio/mnt/himitsu"
  setCurrentDirectory rootDir

  pwd <- getCurrentDirectory >>= makeAbsolute

  L.log $ pack ("Working at '" ++ pwd ++ "'")
  
  cont <- timedLog "Generating file status report"
    initialize

  when cont $ do
    old <- System.IO.readFile zouFile
    new <- System.IO.readFile zouRunFile

    let diffs = diffLines (lines old) (lines new) []
    let faz (fd, st) = putStrLn $ show (st::FileStatus) ++ " " ++ fdName fd
    timedLog "Generating differences" $
      mapM_ faz diffs
  
