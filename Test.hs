module Test where

import Data.Either(isLeft, lefts)

data Type = Unknown | JNumber | JString | JRegex | JArray Type | JObject [(String, Type)] | JFunc [Type] Type
          deriving (Show, Eq)

data Expr = LitNumber Double | LitString String | LitRegex String | LitArray [Expr] | LitObject [(String, Expr)]
	  | Var String -- TODO: perhaps there should only be Property (and for function scopes use a hidden base obj?)
          | Plus Expr Expr | Minus Expr Expr 
          | Assign Expr Expr
          -- | Property Expr String 
          | Index Expr Expr 
          deriving (Show, Eq)

data TypeError = TypeError String Expr [TypeError]
               deriving (Show, Eq)

inferType :: Expr -> Either TypeError Type
inferType (LitNumber _) = Right JNumber
inferType (LitString _) = Right JString
inferType (LitRegex _) = Right JRegex
inferType (LitArray []) = Right $ JArray Unknown
inferType ar@(LitArray (x:xs))
    | areSameType = fmap JArray headType
    | otherwise = Left $ TypeError "array elements are of inconsistent type" ar []
    where headType = inferType x
          areSameType = all (headType ==) $ map inferType xs

inferType obj@(LitObject xs) 
    | any isLeft propTypes = Left $ TypeError "properties are badly typed" obj (lefts propTypes) 
    | otherwise = Right . JObject $ map (\(name, expr) -> (name, fromRight expr)) propNamedTypes
    where propNamedTypes = map (\(name, expr) -> (name, inferType expr)) xs
          propTypes = map snd propNamedTypes
inferType expr = Left $ TypeError "not implemented" expr []

fromRight :: Either a b -> b
fromRight (Right x) = x
fromRight _ = error "Expected Right" 
