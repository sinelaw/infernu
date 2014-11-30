module Demo where

import           Data.Functor                ((<$>))
import           System.Environment          (getArgs)
import           Main                        (checkFiles)

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _ = False

main :: IO ()
main = do
  args <- getArgs
  let [shouldPass, fileName] = args
  res <- fmap last <$> checkFiles [fileName]
  --print $ fmap (pretty . snd) res
  putStrLn $ "// " ++ if isRight res
                      then if shouldPass == "y"
                           then "OK"
                           else "FAIL"
                      else if shouldPass == "y"
                           then "FAIL"
                           else "OK"
