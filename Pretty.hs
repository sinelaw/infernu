module Pretty where

import Data.Maybe(fromMaybe)
import Control.Arrow(second)

import Types


commafy :: [String] -> String
commafy [] = []
commafy [x] = x
commafy (x:xs) = x ++ ", " ++ commafy xs

toJs :: (a -> String) -> Expr a -> String
toJs aToJs = toJs' aToJs 0

-- toJsAnnotated :: InferredStatement -> String
-- toJsAnnotated 

makeTab :: Int -> String
makeTab tabAmount = "\n" ++ concat (replicate tabAmount "  " )

incomment :: String -> String
incomment s = "/* " ++ s ++ " */"

toJsSt :: (a -> String) -> Int -> Statement (Expr a) -> String
toJsSt aToJs tabAmount st = let tab = makeTab tabAmount in
    (tab ++) . (++ ";") $ case st of 
      Empty -> ""
      Expression e -> toJs' aToJs tabAmount e
      Return e -> maybe "" exprAToJs e ++ "return" ++ maybe "" (\x -> " " ++ toJs' aToJs tabAmount x) e
      IfThenElse expr stThen stElse -> 
          concat [ "if (" 
                 , toJs' aToJs tabAmount expr
                 , ") {"
                 , toJsSt' stThen
                 , tab, "} else {"
                 , toJsSt' stElse
                 , tab, "}"]
      While expr stmt -> concat [ "while ("
                                , toJs' aToJs tabAmount expr
                                , ") {"
                                , toJsSt' stmt
                                , tab, "}" ]
      Block stmts -> "{" ++ concatMap toJsSt' stmts ++ tab ++ "}"
    where toJsSt' = toJsSt aToJs (tabAmount + 1)  
          exprAToJs (Expr _ a) = aToJs a ++ makeTab tabAmount

toJs' :: (a -> String) -> Int -> Expr a -> String
toJs' aToJs tabAmount (Expr body a) = let tab = makeTab tabAmount in
    aToJs a ++ case body of
      Assign target src -> toJs'' target ++ " = " ++ toJs'' src
      Call callee args -> toJs'' callee ++ "(" ++ commafy (map toJs'' args) ++ ")"
      Index arr idx -> toJs'' arr ++ "[" ++ toJs'' idx ++ "]"
      LitArray xs -> "[" ++ commafy (map toJs'' xs) ++ "]"
      LitBoolean x -> if x then "true" else "false"
      LitFunc name args vars funcBody -> "function " ++ fromMaybe "" name ++ "(" ++ argsJs ++ ") {" ++ vars' ++ tab ++ statements ++ tab ++ "}"
          where argsJs = commafy args
                vars' = concatMap ((++ (";" ++ tab)) . ("var " ++)) vars
                statements = toJsSt aToJs (tabAmount + 1) funcBody
      LitNumber x -> toJsNumberStr x
      LitObject xs -> "{ " ++ commafy (map (\(name, val) -> name ++ ": " ++ toJs'' val) xs) ++ " }"
      LitRegex regex -> "/" ++ regex ++ "/" -- todo correctly
      LitString s -> "'" ++ s ++ "'" -- todo escape
      Property obj name -> toJs'' obj ++ "." ++ name
      Var name -> name
    where toJs'' = toJs' aToJs (tabAmount + 1)

toJsNumberStr :: (Show a, RealFrac a) => a -> String
toJsNumberStr x = if fromIntegral truncated == x 
                  then show $ truncated
                  else show $ x
    where truncated = truncate x :: Integer

toJsDoc :: JSType -> String
toJsDoc JSUndefined = "undefined"
toJsDoc JSBoolean = "boolean"
toJsDoc JSNumber = "number"
toJsDoc JSString = "string"
toJsDoc JSRegex = "regex"
toJsDoc (JSFunc args res) = "function(" ++ (commafy . map toJsDoc $ args) ++ ") : " ++ toJsDoc res
toJsDoc (JSArray elem') = "[" ++ toJsDoc elem' ++ "]"
toJsDoc (JSObject props) = "{ " ++ (commafy . map showProp $ props) ++ " }"
    where showProp (name, t) = show name ++ ": " ++ toJsDoc t
toJsDoc (JSTVar name) = toStrName name
    where toStrName x = letters!!(x `mod` numLetters):[] ++ suffix x
          letters = ['a'..'z']
          numLetters = length letters
          suffix x = if 0 < x `div` numLetters
                   then show (x `div` numLetters)
                   else ""
                                  


flattenBlocks' :: Expr a -> Expr a
flattenBlocks' (Expr body x) = Expr b x 
    where b = case body of
                LitFunc name args vars stmts -> LitFunc name args vars $ flattenBlocks stmts
                LitArray exprs -> LitArray $ map flattenBlocks' exprs
                LitObject props -> LitObject $ map (second flattenBlocks') props
                Call expr exprs -> Call (flattenBlocks' expr) (map flattenBlocks' exprs)
                Assign e1 e2 -> Assign (flattenBlocks' e1) (flattenBlocks' e2)
                Property e1 name -> Property (flattenBlocks' e1) name
                Index e1 e2 -> Index (flattenBlocks' e1) (flattenBlocks' e2)
                body' -> body'

flattenBlocks :: Statement (Expr a) -> Statement (Expr a)
flattenBlocks (Block xs) = case map flattenBlocks xs of
                             [] -> Empty
                             [x] -> x
                             xs' -> Block xs'
flattenBlocks (While expr stmt) = While (flattenBlocks' expr) (flattenBlocks stmt)
flattenBlocks (IfThenElse expr s1 s2) = IfThenElse (flattenBlocks' expr) (flattenBlocks s1) (flattenBlocks s2)
flattenBlocks x = x

