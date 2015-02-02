{-# LANGUAGE TemplateHaskell, TypeSynonymInstances, FlexibleInstances #-}
module Test where

import Inferno.Types

main :: IO ()
main = do
  res <- runAllTests
  print res
  return ()
  
