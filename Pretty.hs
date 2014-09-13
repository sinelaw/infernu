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
instance (Out a) => Out (Statement a)

commafy :: [String] -> String
commafy [] = []
commafy (x:[]) = x
commafy (x:xs) = x ++ ", " ++ (commafy xs)

toJs :: (a -> String) -> Expr a -> String
toJs aToJs = toJs' aToJs 0

-- toJsAnnotated :: InferredStatement -> String
-- toJsAnnotated 

makeTab :: Int -> String
makeTab tabAmount = "\n" ++ (concat $ replicate tabAmount "  " )

incomment :: String -> String
incomment s = "/* " ++ s ++ " */"

toJsSt :: (a -> String) -> Int -> Statement (Expr a) -> String
toJsSt aToJs tabAmount st = let tab = makeTab tabAmount in
    (tab ++) . (++ ";") $ case st of 
      Empty -> ""
      Expression e -> toJs' aToJs tabAmount e
      Return e -> "return" ++ (maybe "" (\x -> " " ++ (toJs' aToJs tabAmount x)) e)
      IfThenElse expr stThen stElse -> 
          concat [ "if (" 
                 , toJs' aToJs tabAmount expr
                 , ") {"
                 , toJsSt' stThen
                 , tab, "} else {"
                 , toJsSt' stElse
                 , tab, "}"]
      VarDecl name -> "var " ++ name
      While expr stmt -> concat [ "while ("
                                , toJs' aToJs tabAmount expr
                                , ") {"
                                , toJsSt' stmt
                                , tab, "}" ]
      Block stmts -> "{" ++ (concat . map toJsSt' $ stmts) ++ tab ++ "}"
    where toJsSt' = toJsSt aToJs (tabAmount + 1)  
--      _ -> "statement..." -- todo

toJs' :: (a -> String) -> Int -> Expr a -> String
toJs' aToJs tabAmount (Expr body a) = let tab = makeTab tabAmount in
    (aToJs a) ++ case body of
      Assign target src -> (toJs'' target) ++ " = " ++ (toJs'' src)
      Call callee args -> (toJs'' callee) ++ "(" ++ (commafy $ map toJs'' args) ++ ")"
      Index arr idx -> (toJs'' arr) ++ "[" ++ (toJs'' idx) ++ "]"
      LitArray xs -> "[" ++ (commafy $ map toJs'' xs) ++ "]"
      LitBoolean x -> if x then "true" else "false"
      LitFunc name args exprs -> "function " ++ maybe "" show name ++ "(" ++ argsJs ++ ") {" ++ statements ++ tab ++ "}"
          where argsJs = commafy $ args
                statements = concat $ map (toJsSt aToJs (tabAmount + 1)) exprs
      LitNumber x -> toJsNumberStr x
      LitObject xs -> "{ " ++ (commafy $ map (\(name, val) -> name ++ ": " ++ (toJs'' val)) xs) ++ " }"
      LitRegex regex -> "/" ++ regex ++ "/" -- todo correctly
      LitString s -> "'" ++ s ++ "'" -- todo escape
      Property obj name -> (toJs'' obj) ++ "." ++ name
      Var name -> name
    where toJs'' = toJs' aToJs (tabAmount + 1)

toJsNumberStr :: (Show a, RealFrac a) => a -> String
toJsNumberStr x = if (fromIntegral truncated) == x 
                  then show $ truncated
                  else show x
    where truncated = truncate x :: Integer

toJsDoc :: JSType -> String
toJsDoc JSUndefined = "undefined"
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
                                  

flattenBlocks :: Statement a -> Statement a
flattenBlocks (Block xs) = case map flattenBlocks xs of
                             [] -> Empty
                             [x] -> x
                             xs' -> Block xs'
flattenBlocks x = x
      
