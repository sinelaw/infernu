module Main where

import           Control.Arrow                    (second)
import           Data.Functor       ((<$>))
import qualified Data.Set                         as Set
import           System.Environment (getArgs)
import qualified Language.ECMAScript3.Parser      as ES3Parser
import qualified Text.Parsec.Pos                  as Pos

-- TODO move zipByPos and indexList to new module "pretty"
import           Parse(zipByPos, translate, indexList)
-- TODO move pretty stuff to Pretty module
import           Infer(runTypeInference, getAnnotations, TypeError, pretty, Type(..), TBody(..)) 

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _ = False


parseFile :: String -> IO (Either TypeError [(Pos.SourcePos, Type TBody)])
parseFile arg = do
  js <- ES3Parser.parseFromFile arg
  let expr = translate js
      expr' = runTypeInference expr
      res = fmap getAnnotations expr'
      prettyRes = fmap (Set.toList . Set.fromList . fmap (second pretty)) res
  --print js
  sourceCode <- lines <$> readFile arg
  let annotatedSource = case prettyRes of
                          Left e -> pretty e
                          Right xs -> unlines $ zipByPos xs indexedSource
                              where indexedSource = indexList sourceCode
  putStrLn annotatedSource
  return res

main :: IO ()
main = do
  args <- getArgs
  let [shouldPass, fileName] = args
  res <- fmap last <$> parseFile fileName
  --print $ fmap (pretty . snd) res
  putStrLn $ "// " ++ if isRight res
                      then if shouldPass == "y"
                           then "OK"
                           else "FAIL"
                      else if shouldPass == "y"
                           then "FAIL"
                           else "OK"
