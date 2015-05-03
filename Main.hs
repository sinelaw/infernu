module Main (main) where

import           Control.Monad      (forM)
import           SafeJS.Infer       (pretty)
import           SafeJS.Util
import           System.Environment (getArgs)
import qualified Text.Parsec.Pos    as Pos
import SafeJS.Pretty (Pretty(..))
import SafeJS.Types (Type)

process :: Pretty a
           => Either a [(Pos.SourcePos, Type)]
           -> [(Pos.SourceName, [String])]
           -> String
process res sourceCodes =
  case res of
   Right ts -> concatMap (\(f, ds) -> annotatedSource (filteredTypes f ts) ds) sourceCodes
     where filteredTypes f' ts' = filter (\(p, _) -> Pos.sourceName p == f') ts'
   Left e -> pretty e

main :: IO ()
main = do
  files <- getArgs
  res <- checkFiles files
  sourceCodes <- forM files $ \f -> do d <- readFile f
                                       return (f, lines d)

  putStrLn $ process res sourceCodes
  -- TODO exit -1 if error
