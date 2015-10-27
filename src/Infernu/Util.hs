{-# LANGUAGE CPP #-}
module Infernu.Util (checkFiles, annotatedSource, checkSource) where

import           Control.Monad               (forM, when)
import           Data.Maybe                  (catMaybes)
import           Data.List                   (intercalate)
import qualified Data.Set                    as Set
import qualified Language.ECMAScript5.Parser as ES5Parser
import qualified Language.ECMAScript5.PrettyPrint as ES5Pretty
import qualified Language.ECMAScript5.Syntax as ES5
import qualified Language.ECMAScript5.ParserState as ES5PS
import qualified Text.Parsec.Pos             as Pos
import           Text.PrettyPrint.ANSI.Leijen (Pretty (..), (<+>), string, renderPretty, Doc, displayS)

import           Infernu.Prelude
import           Infernu.Options             (Options(..))
import           Infernu.Parse               (translate)
-- TODO move pretty stuff to Pretty module
import           Infernu.Infer               (getAnnotations, minifyVars, runTypeInference)
import           Infernu.Types               (QualType)
import           Infernu.Source              (GenInfo(..), Source(..), TypeError(..), SourcePosSpan(..))

zipByPos :: [(SourcePosSpan, String)] -> [(Int, String)] -> [String]
zipByPos [] xs = map snd xs
zipByPos _  [] = []
zipByPos ps'@((SourcePosSpan pos _end, s):ps) xs'@((i,x):xs) =
    if Pos.sourceLine pos == i
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
checkSource src = case ES5Parser.parseFromString src of
                   Left parseError -> Left $ TypeError { source = Source (GenInfo True Nothing, SourcePosGlobal), message = string (show parseError) }
                   Right expr ->
                       -- case ES5.isValid expr of
                       --     False -> Left $ TypeError { source = Source (GenInfo True, SourcePosGlobal), message = "Invalid syntax" }
                       --     True ->
                       fmap getAnnotations
                       $ fmap minifyVars
                       $ runTypeInference
                       $ fmap (Source)
                       $ translate
                       $ map (fmap getSource)
                       $ ES5.unProgram expr

getSource (ES5PS.SourceSpan (a, b), _) = SourcePosSpan a b

checkFiles :: Options -> [String] -> IO (Either TypeError [(Source, QualType)])
checkFiles options fileNames = do
  expr <- concatMap ES5.unProgram <$> forM fileNames ES5Parser.parseFromFile
  when (optShowParsed options) $ putStrLn $ show $ ES5Pretty.prettyPrint expr
  let expr' = fmap Source $ translate $ map (fmap getSource) expr
  when (optShowCore options) $ putStrLn $ show $ pretty expr'
  let expr'' = fmap minifyVars $ runTypeInference expr'
      res = fmap getAnnotations expr''
  return res

showWidth :: Int -> Doc -> String
showWidth w x   = displayS (renderPretty 0.4 w x) ""

showDoc :: Doc -> String
showDoc = showWidth 100

annotatedSource :: [(Source, QualType)] -> [String] -> String
annotatedSource xs sourceCode = unlines $ zipByPos (prettyRes $ unGenInfo $ filterGen xs) indexedSource
  where indexedSource = indexList sourceCode
        unGenInfo :: [(Source, QualType)] -> [(String, SourcePosSpan, QualType)]
        unGenInfo = catMaybes . map (\(Source (g, s), q) -> fmap (\n -> (n, s, q)) $ declName g)
        filterGen :: [(Source, QualType)] -> [(Source, QualType)]
        filterGen = filter (\(Source (g, _), _) -> not . isGen $ g)
        prettyRes = Set.toList . Set.fromList . fmap (\(n, s, q) -> (s, showDoc $ pretty n <+> string ":" <+> pretty q))
