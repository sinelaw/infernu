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
  let (shouldPass:fileName:[]) = args
  res <- parseFile fileName
  putStrLn $ if isRight res 
             then if shouldPass == "y" 
                  then "OK"
                  else "FAIL"
             else if shouldPass == "y"
                  then "FAIL"
                  else "OK"
