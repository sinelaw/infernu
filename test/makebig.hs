module Main where

main = do
  putStrLn "var a0=0;"
  mapM_ (\x -> putStrLn $ "var a"++show x++"=1+a"++show (x-1)++";") [1..2000]

