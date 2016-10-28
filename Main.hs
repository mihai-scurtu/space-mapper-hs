module Main
  where

import System.Environment (getArgs)
import SpaceMapper (buildSpaceMap, prettyPrint)

main = do
  args <- getArgs
  case args of
    [dir] -> do
      spaceMap <- buildSpaceMap dir
      prettyPrint spaceMap
      return ()
    _ -> putStrLn "Error: Please specify a parameter."
