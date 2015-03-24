module Main (main) where

import           Control.Monad       (forM)
import qualified Options.Applicative as OptParse
import           System.Exit         (exitFailure)
import qualified Text.Parsec.Pos     as Pos

import           Infernu.Infer       (pretty)
import           Infernu.Options     (Options (..), opts)
import           Infernu.Pretty      (Pretty (..))
import           Infernu.Types       (QualType, Source (..))
import           Infernu.Util

process :: Pretty a => Either a [(Source, QualType)] -> [(Pos.SourceName, [String])] -> Either String String
process res sourceCodes =
    case res of
        Right ts -> Right $ concatMap (\(f, ds) -> annotatedSource (filteredTypes f ts) ds) sourceCodes
           where filteredTypes f' = filter (\(Source (_, p), _) -> (Pos.sourceName p == f'))
        Left e -> Left $ pretty e

main :: IO ()
main = do
    options <- OptParse.execParser opts
    let files = optFileNames options
    res <- checkFiles (optShowCore options) files
    sourceCodes <- forM files $ \f -> do d <- readFile f
                                         return (f, lines d)

    case process res sourceCodes of
        Right s -> putStrLn s
        Left e -> putStrLn e >> exitFailure

