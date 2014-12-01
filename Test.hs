{-# LANGUAGE TemplateHaskell, TypeSynonymInstances, FlexibleInstances #-}
module Test where

import Infer

main = do
  runAllTests
  return ()
  
