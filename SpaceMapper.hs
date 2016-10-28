module SpaceMapper
  ( buildSpaceMap
  , prettyPrint
  )
  where

import System.Directory (doesDirectoryExist, listDirectory, doesFileExist, getFileSize, getCurrentDirectory)
import Control.Monad (mapM, mapM_)
import Data.Tuple (fst, snd)
import Numeric (showFFloat)
import Control.Exception (catch, IOException)

type Entry = (FilePath, Integer)

type SpaceMap = [Entry]

buildSpaceMap :: FilePath -> IO (SpaceMap)
buildSpaceMap dir = do
    isDir <- doesDirectoryExist dir
    if isDir
    then
      do
        cwd <- getCurrentDirectory
        contents <- listDirectory dir
        sizes <- mapM (getSize $ cwd ++ "/" ++  dir) contents

        return $ zip contents sizes
    else
      undefined


getSize :: FilePath -> FilePath -> IO (Integer)
getSize prefixPath fileOrDir = do
    isFile <- doesFileExist fullPath

    if isFile
    then
        catch (getFileSize fullPath) sizeHandler
    else
      do
        contents <- catch (listDirectory fullPath) contentHandler
        sizes <- mapM (getSize fullPath) contents
        return $ foldr (+) 0 sizes
  where
    fullPath = prefixPath ++ "/" ++ fileOrDir

    sizeHandler :: IOException -> IO (Integer)
    sizeHandler e = do
      -- putStrLn $ "Caught exception: " ++ show e
      return 0

    contentHandler :: IOException -> IO ([a])
    contentHandler e = do
      -- putStrLn $ "Caught exception: " ++ show e
      return []

prettyPrint :: SpaceMap -> IO ()
prettyPrint smap =
  let
    maxSize = maximum $ map length $ map (prettyFileSize . snd) smap

    padding :: Int -> String -> String
    padding maxLength s = take (maxLength - (length s) + 2) $ repeat ' '


    lineString :: Entry -> String
    lineString (name, size) =
      let
        prettySize = prettyFileSize size
      in
        (prettySize) ++ (padding maxSize prettySize) ++ name
  in
    mapM_ (putStrLn . lineString) smap

prettyFileSize :: Integer -> String
prettyFileSize size
    | size < 1024                       = show size ++ " B"
    | size < 1024 * 1024                = (prettyFloat ((fromIntegral size) / 1024)) ++ " KB"
    | size < 1024 * 1024 * 1024         = (prettyFloat ((fromIntegral size) / (1024 * 1024))) ++ " MB"
    | otherwise                         = (prettyFloat ((fromIntegral size) / (1024 * 1024 * 1024)))  ++ " GB"

prettyFloat :: RealFloat a => a -> String
prettyFloat x = showFFloat (Just 2) x ""
