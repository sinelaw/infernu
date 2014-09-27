module Main where

import System.Environment(getArgs)
import Text.PrettyPrint.GenericPretty(pp, Out(..))
import qualified Language.ECMAScript3.Syntax as ES3
import qualified Language.ECMAScript3.Parser as ES3Parser

import Types
import Pretty
import Infer
import Parse

-- printInferredExprType :: Either TypeError InferredExpr -> String
-- printInferredExprType (Right t) = incomment . toJsDoc . exprData $ t
-- printInferredExprType (Left x) = incomment . show $ x

printType :: Statement (Expr JSType) -> IO ()
printType stmt = putStrLn $ toJsSt (incomment . toJsDoc) 0 stmt

instance (Out a) => Out (Body a)
instance (Out a) => Out (Expr a)
instance (Out a) => Out (Statement a)
instance (Out a) => Out (TypeError a)

main :: IO ()
main = do
  args <- getArgs
  let arg = head args
  js <- ES3Parser.parseFromFile arg 
  putStrLn . show $ js
  let stmts = map fromStatement $ ES3.unJavaScript js
--  pp $ Block . flattenBlocks $ stmts
  let input = flattenBlocks . Block $ stmts
  -- pp input
  -- putStrLn "\n---------------\n"
  let inf = runInfer $ inferStatement input
  putStrLn "\n---------------\n"
  pp inf
  case inf of
    Left err' -> print err'
    Right inf' -> printType inf'
--  toJsSt $ fst inf


-- idE = ex $ LitFunc ["arg"] [ Return . Just . ex $ Var "arg", VarDecl "var1", st $ Assign (ex $ Var "var1") (ex $ LitNumber 1), Return . Just . ex $ Var "arg" ]
-- idT = runState (inferType idE) emptyScope

-- e1 = ex $ LitFunc ["arg"]
--      $ [ VarDecl "vari"
--        , st $ Assign (ex $ Var "vari") (ex $ LitObject [("amount", ex $ LitNumber 123)])
--        , While (ex $ LitBoolean False) (st $ Assign (ex $ Property (ex $ Var "vari") "amount") (ex $ LitNumber 0))
--    --    , ex $ Assign (ex $ Var "vari") (ex $ LitString "ma?")
--        , IfThenElse (ex $ LitBoolean False) (Return $ Just . ex $ LitArray []) (Return $ Just . ex $ LitArray [ex $ LitObject [("bazooka", ex $ Var "arg"), ("number", ex $ Var "vari")]])]
-- --e1 = ex $ LitFunc ["arg"] ["vari"] []

-- t1 = inferType e1
-- s1 = runState t1 emptyScope
-- s1doc = toJsDoc . fromJust . getExprType $ fst s1

-- e2 = ex $ Property (ex $ Index (ex $ Call e1 [(ex $ LitString "abc")]) (ex $ LitNumber 2)) "number"
-- s2 = runState (inferType e2) emptyScope


-- e3 = ex $ Assign (ex $ Var "f") e1
-- s3 = runState (inferType e3) emptyScope

-- arrayTest = ex $ Index (ex $ LitArray [ex $ LitBoolean False, ex $ LitBoolean True]) (ex $ LitNumber 32)

-- infArrayTest = runInfer $ inferExpr arrayTest

-- funcTest = ex $ LitFunc (Just "myFunc") ["x", "y"] [Expression . ex $ Call (ex $ Var "x") [(ex $ Var "y")]]

-- infFuncTest = runInfer $ inferExpr funcTest

-- failFuncTest = ex $ LitFunc (Just "myFunc") ["x"] [Expression . ex $ Call (ex $ Var "x") [ex $ Var "x"]]

-- infFailFuncTest = runInfer $ inferExpr failFuncTest

