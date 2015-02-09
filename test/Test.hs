{-# LANGUAGE TemplateHaskell, TypeSynonymInstances, FlexibleInstances #-}
module Test where

import Infernu.Types

main :: IO ()
main = do
  res <- runAllTests
  print res
  return ()
  
