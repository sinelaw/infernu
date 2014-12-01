{-# LANGUAGE TemplateHaskell, TypeSynonymInstances, FlexibleInstances #-}
module Test where

import SafeJS.Infer

main = do
  runAllTests
  return ()
  
