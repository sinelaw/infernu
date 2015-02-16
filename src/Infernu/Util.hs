{-# LANGUAGE CPP #-}
module Infernu.Util (checkFiles, annotatedSource, checkSource) where

import           Control.Arrow               (second)
import           Control.Monad               (forM)
import           Data.Functor                ((<$>))
import qualified Data.Set                    as Set
import qualified Language.ECMAScript3.Parser as ES3Parser
import qualified Language.ECMAScript3.Syntax as ES3
import qualified Text.Parsec.Pos             as Pos

import           Infernu.Parse                (translate)
-- TODO move pretty stuff to Pretty module
import           Infernu.Infer                (getAnnotations, runTypeInference, minifyVars)
import           Infernu.Pretty               (pretty)
import           Infernu.Types                (TypeError(..), QualType)

zipByPos :: [(Pos.SourcePos, String)] -> [(Int, String)] -> [String]
zipByPos [] xs = map snd xs
zipByPos _  [] = []
zipByPos ps'@((pos, s):ps) xs'@((i,x):xs) = if Pos.sourceLine pos == i
                                            then ("//" ++ indentToColumn (Pos.sourceColumn pos) ++ s) : zipByPos ps xs'
                                            else x : zipByPos ps' xs
    where indentToColumn n = replicate (n - 3) ' '


indexList :: [a] -> [(Int, a)]
indexList = zip [1..]


checkSource :: String -> Either TypeError [(Pos.SourcePos, QualType)]
checkSource src = case ES3Parser.parseFromString src of
                   Left parseError -> Left $ TypeError { source = Pos.initialPos "<global>", message = show parseError }
                   Right expr -> fmap getAnnotations $ fmap minifyVars $ runTypeInference $ translate $ ES3.unJavaScript expr

checkFiles :: [String] -> IO (Either TypeError [(Pos.SourcePos, QualType)])
checkFiles fileNames = do
  expr <- concatMap ES3.unJavaScript <$> forM fileNames ES3Parser.parseFromFile
  let expr' = translate $ expr
      expr'' = fmap minifyVars $ runTypeInference expr'
      res = fmap getAnnotations expr''
#ifdef TRACE
  putStrLn $ pretty expr'
#endif
  return res

annotatedSource :: [(Pos.SourcePos, QualType)] -> [String] -> String
annotatedSource xs sourceCode = unlines $ zipByPos prettyRes indexedSource
  where indexedSource = indexList sourceCode
        prettyRes = (Set.toList . Set.fromList . fmap (second pretty)) xs
