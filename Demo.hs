module Main where

import System.Exit(exitWith, ExitCode(..))
import System.Environment(getArgs)    
import Infer
import Parse

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _ = False

main :: IO ()
main = do
  args <- getArgs
  let (shouldFail:fileName:[]) = args
  res <- parseFile fileName
  putStrLn $ if isRight res 
             then if shouldFail == "y" 
                  then "FAIL"
                  else "OK"
             else if shouldFail == "y"
                  then "OK"
                  else "FAIL"
