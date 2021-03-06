{-# LANGUAGE CPP #-}
module Infernu.Util (checkFiles, annotatedSource, checkSource) where

import           Control.Monad               (forM, when)
import           Data.Maybe                  (catMaybes)
import           Data.List                   (intercalate)
import qualified Data.Set                    as Set
import qualified Language.ECMAScript3.Parser as ES3Parser
import qualified Language.ECMAScript3.PrettyPrint as ES3Pretty
import qualified Language.ECMAScript3.Syntax as ES3
import qualified Text.Parsec.Pos             as Pos
import           Text.PrettyPrint.ANSI.Leijen (Pretty (..), (<+>), string, renderPretty, Doc, displayS)

import           Infernu.Prelude
import           Infernu.Options             (Options(..))
import           Infernu.Parse               (translate)
-- TODO move pretty stuff to Pretty module
import           Infernu.Infer               (getAnnotations, minifyVars, runTypeInference)
import           Infernu.Types               (QualType)
import           Infernu.Source              (GenInfo(..), Source(..), TypeError(..))

zipByPos :: [(Pos.SourcePos, String)] -> [(Int, String)] -> [String]
zipByPos [] xs = map snd xs
zipByPos _  [] = []
zipByPos ps'@((pos, s):ps) xs'@((i,x):xs) = if Pos.sourceLine pos == i
                                            then formattedAnnotation : zipByPos ps xs'
                                            else x : zipByPos ps' xs
    where indentToColumn n = replicate (n-1) ' '
          isMultiline = length sLines > 1
          sLines = lines s
          formattedAnnotation = if isMultiline
                                then ("/*"
                                      ++ indentToColumn (Pos.sourceColumn pos - 2)
                                      ++ head sLines
                                      ++ "\n"
                                      ++ (intercalate "\n" . map (\l -> indentToColumn (Pos.sourceColumn pos) ++ l) $ tail sLines) ++ " */")
                                else "//" ++ indentToColumn (Pos.sourceColumn pos - 2) ++ s


indexList :: [a] -> [(Int, a)]
indexList = zip [1..]


checkSource :: String -> Either TypeError [(Source, QualType)]
checkSource src = case ES3Parser.parseFromString src of
                   Left parseError -> Left $ TypeError { source = Source (GenInfo True Nothing, Pos.initialPos "<global>"), message = string (show parseError) }
                   Right expr -> -- case ES3.isValid expr of
                                 --     False -> Left $ TypeError { source = Source (GenInfo True, Pos.initialPos "<global>"), message = "Invalid syntax" }
                                 --     True ->
                                 fmap getAnnotations $ fmap minifyVars $ runTypeInference $ fmap Source $ translate $ ES3.unJavaScript expr

checkFiles :: Options -> [String] -> IO (Either TypeError [(Source, QualType)])
checkFiles options fileNames = do
  expr <- concatMap ES3.unJavaScript <$> forM fileNames ES3Parser.parseFromFile
  when (optShowParsed options) $ putStrLn $ show $ ES3Pretty.prettyPrint expr
  let expr' = fmap Source $ translate $ expr
  when (optShowCore options) $ putStrLn $ show $ pretty expr'
  let expr'' = fmap minifyVars $ runTypeInference expr'
      res = fmap getAnnotations expr''
  return res

showWidth :: Int -> Doc -> String
showWidth w x   = displayS (renderPretty 0.4 w x) ""

showDoc :: Doc -> String
showDoc = showWidth 120

annotatedSource :: [(Source, QualType)] -> [String] -> String
annotatedSource xs sourceCode = unlines $ zipByPos (prettyRes $ unGenInfo $ filterGen xs) indexedSource
  where indexedSource = indexList sourceCode
        unGenInfo :: [(Source, QualType)] -> [(String, Pos.SourcePos, QualType)]
        unGenInfo = catMaybes . map (\(Source (g, s), q) -> fmap (\n -> (n, s, q)) $ declName g)
        filterGen :: [(Source, QualType)] -> [(Source, QualType)]
        filterGen = filter (\(Source (g, _), _) -> not . isGen $ g)
        prettyRes = Set.toList . Set.fromList . fmap (\(n, s, q) -> (s, showDoc $ pretty n <+> string ":" <+> pretty q))
