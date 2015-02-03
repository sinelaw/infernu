module Inferno.Lib where


matchZip :: [a] -> [b] -> Maybe [(a,b)]
matchZip xs ys = if length xs /= length ys
                 then Nothing
                 else Just $ zip xs ys
                      
