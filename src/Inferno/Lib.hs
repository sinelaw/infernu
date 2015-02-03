module Inferno.Lib where

import           Data.Maybe                (fromMaybe)


matchZip :: [a] -> [b] -> Maybe [(a,b)]
matchZip [] [] = Just []
matchZip (_:_) [] = Nothing
matchZip [] (_:_) = Nothing
matchZip (x:xs) (y:ys) = fmap ((x,y):) $ matchZip xs ys

safeLookup :: Eq a => [(a,a)] -> a -> a
safeLookup assoc n = fromMaybe n $ lookup n assoc

