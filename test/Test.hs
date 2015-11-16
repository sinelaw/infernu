{-# LANGUAGE TemplateHaskell, TypeSynonymInstances, FlexibleInstances #-}
module Test where

import Infernu.Types
import Infernu.Prelude

main :: IO ()
main = do
  res <- runAllTests
  print res
  return ()

