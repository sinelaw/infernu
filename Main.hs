{-# LANGUAGE CPP               #-}

module Main (main, checkFiles) where

import           Control.Arrow               (second)
import           Control.Monad               (forM)
import           Data.Functor                ((<$>))
import qualified Data.Set                    as Set
import qualified Language.ECMAScript3.Parser as ES3Parser
import qualified Language.ECMAScript3.Syntax as ES3
import           System.Environment          (getArgs)
import qualified Text.Parsec.Pos             as Pos

import           Parse                       (translate)
-- TODO move pretty stuff to Pretty module
import           Infer                       (TBody (..), Type (..), TypeError,
                                              getAnnotations, pretty,
                                              runTypeInference)

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _ = False

zipByPos :: [(Pos.SourcePos, String)] -> [(Int, String)] -> [String]
zipByPos [] xs = map snd xs
zipByPos _  [] = []
zipByPos ps'@((pos, s):ps) xs'@((i,x):xs) = if Pos.sourceLine pos == i
                                            then ("//" ++ indentToColumn (Pos.sourceColumn pos) ++ s) : zipByPos ps xs'
                                            else x : zipByPos ps' xs
    where indentToColumn n = replicate (n - 3) ' '


indexList :: [a] -> [(Int, a)]
indexList = zip [1..]


checkFiles :: [String] -> IO (Either TypeError [(Pos.SourcePos, Type TBody)])
checkFiles fileNames = do
  expr <- concatMap ES3.unJavaScript <$> forM fileNames ES3Parser.parseFromFile
  let expr' = translate $ expr
      expr'' = runTypeInference expr'
      res = fmap getAnnotations expr''
#ifdef TRACE      
  putStrLn $ pretty expr'
#endif  
  return res

annotatedSource :: [(Pos.SourcePos, Type TBody)] -> [String] -> String
annotatedSource xs sourceCode = unlines $ zipByPos prettyRes indexedSource
  where indexedSource = indexList sourceCode
        prettyRes = (Set.toList . Set.fromList . fmap (second pretty)) xs

main :: IO ()
main = do
  files <- getArgs
  res <- checkFiles files
  sourceCodes <- forM files $ \f -> do d <- readFile f
                                       return (f, lines d)
  let source' = case res of
                 Right ts -> concatMap (\(f, ds) -> annotatedSource (filteredTypes f ts) ds) sourceCodes
                 Left e -> pretty e
      filteredTypes f' ts' = filter (\(p, _) -> Pos.sourceName p == f') ts'

  putStrLn source'
  -- TODO exit -1 if error
