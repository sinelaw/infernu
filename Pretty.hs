module Pretty where

import qualified Data.Map.Lazy as Map
import Data.List(intersperse)
import Text.PrettyPrint.GenericPretty(Generic, Out(..), pp)


import Types
import Infer



instance (Out k, Out v) => Out (Map.Map k v) where
    doc m = doc $ Map.assocs m
    docPrec _ = doc

instance (Out a) => Out (Body a)
instance (Out a) => Out (Expr a)
instance Out TypeError
instance Out VarScope
instance Out TypeScope
instance Out FuncScope
instance Out Scope


commafy :: [String] -> String
commafy [] = []
commafy (x:[]) = x
commafy (x:xs) = x ++ ", " ++ (commafy xs)

toJs :: Expr a -> String
toJs = toJs' 0

makeTab :: Int -> String
makeTab tabAmount = "\n" ++ (concat $ replicate tabAmount "  " )

toJs' :: Int -> Expr a -> String
toJs' tabAmount (Expr body _) = let tab = makeTab tabAmount in
    case body of
      Assign target src -> (toJs'' target) ++ " = " ++ (toJs'' src)
      Call callee args -> (toJs'' callee) ++ "(" ++ (commafy $ map toJs'' args) ++ ")"
      Index arr idx -> (toJs'' arr) ++ "[" ++ (toJs'' idx) ++ "]"
      LitArray xs -> "[" ++ (commafy $ map toJs'' xs) ++ "]"
      LitBoolean x -> if x then "true" else "false"
      LitFunc args varNames exprs -> "function (" ++ argsJs ++ ") {" ++ block ++ tab ++ "}"
          where argsJs = commafy $ args
                block = concat $ intersperse tab' ["", vars', "", statements]
                statements = (concat $ map (++ ";" ++ tab') $ map toJs'' exprs)
                vars' = "var " ++ commafy varNames ++ ";"
                tab' = makeTab $ tabAmount + 1
      LitNumber x -> if (fromIntegral truncated) == x 
                     then show $ truncated
                     else show x
          where truncated = truncate x :: Integer
      LitObject xs -> "{ " ++ (commafy $ map (\(name, val) -> name ++ ": " ++ (toJs'' val)) xs) ++ " }"
      LitRegex regex -> "/" ++ regex ++ "/" -- todo correctly
      LitString s -> "'" ++ s ++ "'" -- todo escape
      Property obj name -> (toJs'' obj) ++ "." ++ name
      Return expr -> "return " ++ toJs'' expr
      Var name -> name
    where toJs'' = toJs' (tabAmount + 1)


toJsDoc :: JSType -> String
toJsDoc JSBoolean = "boolean"
toJsDoc JSNumber = "number"
toJsDoc JSString = "string"
toJsDoc JSRegex = "regex"
toJsDoc (JSFunc args res) = "function(" ++ (commafy . map toJsDoc $ args) ++ ") : " ++ (toJsDoc res)
toJsDoc (JSArray elem') = "[" ++ toJsDoc elem' ++ "]"
toJsDoc (JSObject props) = "{ " ++ (commafy . map showProp $ props) ++ " }"
    where showProp (name, t) = (show name) ++ ": " ++ (toJsDoc t)
toJsDoc (JSTVar name) = toStrName name
    where toStrName x = letters!!(x `mod` numLetters):[] ++ (suffix x)
          letters = ['a'..'z']
          numLetters = length letters
          suffix x = if 0 < x `div` numLetters
                   then show (x `div` numLetters)
                   else ""
                                  
