module Demo where

import           Data.List          (intercalate)

import           Text.PrettyPrint.ANSI.Leijen (Pretty (..))
import           Infernu.Util       (checkFiles)
import           Infernu.Options    (defaultOptions)
import           Infernu.Prelude

import           System.Environment (getArgs)

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _ = False

main :: IO ()
main = do
  args <- getArgs
  let [shouldPassS, fileName] = args
  res <- fmap last <$> checkFiles defaultOptions [fileName]
  let shouldPass = if shouldPassS == "y" then id else not
      typeChecked = isRight res
      message = case res of
        Left e -> show $ pretty e
        Right _ -> ""
      toOk = bool "FAIL" "OK" . shouldPass
  --print $ fmap (pretty . snd) res
  putStrLn $ "// " ++ toOk typeChecked ++ " " ++ (intercalate " | " $ lines message)
