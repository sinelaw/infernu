module Demo where

import           Data.Bool          (bool)
import           Data.Functor       ((<$>))
import           Main               (checkFiles)
import           Infer              (pretty)
import           System.Environment (getArgs)

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _ = False

main :: IO ()
main = do
  args <- getArgs
  let [shouldPassS, fileName] = args
  res <- fmap last <$> checkFiles [fileName]
  let shouldPass = if shouldPassS == "y" then id else not
      typeChecked = isRight res
      message = case res of
        Left e -> pretty e
        Right _ -> ""
      toOk = bool "FAIL" "OK" . shouldPass
  --print $ fmap (pretty . snd) res
  putStrLn $ "// " ++ toOk typeChecked ++ " " ++ message
