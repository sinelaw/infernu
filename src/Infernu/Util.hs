{-# LANGUAGE CPP #-}
module Infernu.Util (checkFiles, annotatedSource, checkSource) where

import           Control.Arrow               (second)
import           Control.Monad               (forM, when)
import qualified Data.Set                    as Set
import qualified Language.ECMAScript3.Parser as ES3Parser
import qualified Language.ECMAScript3.PrettyPrint as ES3Pretty
import qualified Language.ECMAScript3.Syntax as ES3
import qualified Text.Parsec.Pos             as Pos

import           Infernu.Options (Options(..))
import           Infernu.Parse               (translate)
-- TODO move pretty stuff to Pretty module
import           Infernu.Infer               (getAnnotations, minifyVars, runTypeInference)
import           Infernu.Pretty              (pretty)
import           Infernu.Types               (IsGen (..), QualType, Source (..), TypeError (..))

zipByPos :: [(Pos.SourcePos, String)] -> [(Int, String)] -> [String]
zipByPos [] xs = map snd xs
zipByPos _  [] = []
zipByPos ps'@((pos, s):ps) xs'@((i,x):xs) = if Pos.sourceLine pos == i
                                            then ("//" ++ indentToColumn (Pos.sourceColumn pos) ++ s) : zipByPos ps xs'
                                            else x : zipByPos ps' xs
    where indentToColumn n = replicate (n - 3) ' '


indexList :: [a] -> [(Int, a)]
indexList = zip [1..]


checkSource :: String -> Either TypeError [(Source, QualType)]
checkSource src = case ES3Parser.parseFromString src of
                   Left parseError -> Left $ TypeError { source = Source (IsGen True, Pos.initialPos "<global>"), message = show parseError }
                   Right expr -> -- case ES3.isValid expr of
                                 --     False -> Left $ TypeError { source = Source (IsGen True, Pos.initialPos "<global>"), message = "Invalid syntax" }
                                 --     True ->
                                 fmap getAnnotations $ fmap minifyVars $ runTypeInference $ fmap Source $ translate $ ES3.unJavaScript expr

checkFiles :: Options -> [String] -> IO (Either TypeError [(Source, QualType)])
checkFiles options fileNames = do
  expr <- concatMap ES3.unJavaScript <$> forM fileNames ES3Parser.parseFromFile
  when (optShowParsed options) $ putStrLn $ show $ ES3Pretty.prettyPrint expr
  let expr' = fmap Source $ translate $ expr
  when (optShowCore options) $ putStrLn $ pretty expr'
  let expr'' = fmap minifyVars $ runTypeInference expr'
      res = fmap getAnnotations expr''
  return res

annotatedSource :: [(Source, QualType)] -> [String] -> String
annotatedSource xs sourceCode = unlines $ zipByPos (prettyRes $ unIsGen $ filterGen xs) indexedSource
  where indexedSource = indexList sourceCode
        unIsGen :: [(Source, QualType)] -> [(Pos.SourcePos, QualType)]
        unIsGen = map (\(Source (_, s), q) -> (s, q))
        filterGen = filter (\(Source (IsGen g, _), _) -> not g)
        prettyRes = Set.toList . Set.fromList . fmap (second pretty)
