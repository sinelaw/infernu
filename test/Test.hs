{-# LANGUAGE TemplateHaskell, TypeSynonymInstances, FlexibleInstances #-}
module Test where

import Inferno.Infer

main :: IO ()
main = do
  res <- runAllTests
  print res
  return ()
  
