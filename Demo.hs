module Main where

import System.Environment(getArgs)    
import Infer
import Parse

main :: IO ()
main = do
  args <- getArgs
  let arg = head args
  parseFile arg
