module Main (main) where

import           Control.Monad       (forM, when)
import qualified Options.Applicative as OptParse
import           System.Exit         (exitFailure)
import qualified Text.Parsec.Pos     as Pos

import           Text.PrettyPrint.ANSI.Leijen (Pretty (..))
import           Infernu.Options     (Options (..), opts)
import           Infernu.Types       (QualType)
import           Infernu.Source      (Source(..))
import           Infernu.Util
import           Infernu.Prelude

process :: [(Source, QualType)] -> [(Pos.SourceName, [String])] -> String
process ts sourceCodes = concatMap (\(f, ds) -> annotatedSource (filteredTypes f ts) ds) sourceCodes
    where filteredTypes f' = filter (\(Source (_, p), _) -> (Pos.sourceName p == f'))

main :: IO ()
main = do
    options <- OptParse.execParser opts
    let files = optFileNames options
    res <- checkFiles options files
    case res of
        Right ts -> when (not $ optQuiet options) $ do sourceCodes <- forM files $ \f -> do d <- readFile f
                                                                                            return (f, lines d)
                                                       putStrLn $ process ts sourceCodes
        Left e -> putStrLn (show $ pretty e) >> exitFailure

