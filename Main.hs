module Main (main) where

import           Control.Monad      (forM)
import           SafeJS.Infer       (pretty)
import           SafeJS.Util
import           System.Environment (getArgs)
import qualified Text.Parsec.Pos    as Pos

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
