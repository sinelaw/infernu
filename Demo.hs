module Main where

import Data.Functor((<$>))    
import System.Environment(getArgs)    
import Parse
import Infer(pretty)
    
isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _ = False

main :: IO ()
main = do
  args <- getArgs
  let (shouldPass:fileName:[]) = args
  res <- fmap (head . reverse) <$> parseFile fileName
  --print $ fmap (pretty . snd) res
  putStrLn $ "// " ++ if isRight res 
                      then if shouldPass == "y" 
                           then "OK"
                           else "FAIL"
                      else if shouldPass == "y"
                           then "FAIL"
                           else "OK"
