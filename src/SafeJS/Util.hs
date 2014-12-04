{-# LANGUAGE CPP #-}
module SafeJS.Util (checkFiles, annotatedSource) where

import           Control.Arrow               (second)
import           Control.Monad               (forM)
import           Data.Functor                ((<$>))
import qualified Data.Set                    as Set
import qualified Language.ECMAScript3.Parser as ES3Parser
import qualified Language.ECMAScript3.Syntax as ES3
import qualified Text.Parsec.Pos             as Pos

import           SafeJS.Parse                (translate)
-- TODO move pretty stuff to Pretty module
import           SafeJS.Infer                (getAnnotations, runTypeInference)
import           SafeJS.Pretty               (pretty)
import           SafeJS.Types                (TBody (..), Type (..), TypeError)

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

