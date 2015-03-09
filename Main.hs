module Main (main) where

import           Control.Monad      (forM)
import           Infernu.Infer      (pretty)
import           Infernu.Pretty     (Pretty (..))
import           Infernu.Types      (QualType, Source)
import           Infernu.Util
import           System.Environment (getArgs)
import           System.Exit        (exitFailure)
import qualified Text.Parsec.Pos    as Pos

process :: Pretty a => Either a [(Source, QualType)] -> [(Pos.SourceName, [String])] -> Either String String
process res sourceCodes =
  case res of
   Right ts -> Right $ concatMap (\(f, ds) -> annotatedSource (filteredTypes f ts) ds) sourceCodes
     where filteredTypes f' ts' = filter (\((_, p), _) -> (Pos.sourceName p == f')) ts'
   Left e -> Left $ pretty e

main :: IO ()
main = do
  files <- getArgs
  res <- checkFiles files
  sourceCodes <- forM files $ \f -> do d <- readFile f
                                       return (f, lines d)

  case process res sourceCodes of
      Right s -> putStrLn s
      Left e -> putStrLn e >> exitFailure

