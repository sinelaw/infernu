module Inferno.Lib where


matchZip :: [a] -> [b] -> Maybe [(a,b)]
matchZip [] [] = Just []
matchZip (_:_) [] = Nothing
matchZip [] (_:_) = Nothing
matchZip (x:xs) (y:ys) = fmap ((x,y):) $ matchZip xs ys

