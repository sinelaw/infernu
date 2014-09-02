module Test where

import Data.Either(isLeft, lefts)

data Type = Unknown | JNumber | JString | JRegex | JArray Type | JObject [(String, Type)] | JFunc [Type] Type
data Expr = LitNumber Double | LitString String | LitRegex String | LitArray [Expr] | LitObject [(String, Expr)]
	  | Var String -- TODO: perhaps there should only be Property (and for function scopes use a hidden base obj?)
          | Plus Expr Expr | Minus Expr Expr 
          | Assign Expr Expr
          -- | Property Expr String 
          | Index Expr Expr 
          deriving (Show, Eq)

data TypeError = TypeError String Expr [TypeError]

inferType :: Expr -> Either TypeError Type
inferType (LitNumber x) = Right JNumber
inferType (LitString x) = Right JString
inferType (LitRegex x) = Right JRegex
inferType (LitArray []) = Right $ JArray Unknown
inferType ar@(LitArray (x:xs)) = if areSameType 
                                 then Right $ JArray headType
                                 else Left "array elements are of inconsistent type" ar
    where headType = inferType x
          areSameType = all (\y -> y == x) $ map inferType xs

inferType obj@(LitObject xs) = if (any isLeft propTypes) 
                               then Left $ TypeError "properties are badly typed" obj (lefts propTypes) 
                               else Right $ JObject propNamedTypes
    where propNamedTypes = map (\name expr -> (name, inferType expr)) xs
          propTypes = map snd propNamedTypes
inferType expr = Left "not implemented" expr
