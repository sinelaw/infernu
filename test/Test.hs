{-# LANGUAGE TemplateHaskell, TypeSynonymInstances, FlexibleInstances #-}
module Test where

import Inferno.Infer

main = do
  runAllTests
  return ()
  
