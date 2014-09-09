module Demo where

import Text.PrettyPrint.GenericPretty(pp)
import Control.Monad.State(runState)
import Data.Maybe(fromJust)

import qualified Language.ECMAScript3.Parser
import qualified Language.ECMAScript3.Syntax as ES3

import Infer
import Pretty
import Types

-- ------------------------------------------------------------------------
--data VarScope = Global | VarScope { parent :: VarScope, vars :: [(String, JSType)] }

fromStatement :: ES3.Statement a -> Statement (Expr ())
fromStatement (ES3.BlockStmt _ stmts) = st $ Call (ex $ LitFunc [] [] $ map fromStatement stmts) []
fromStatement (ES3.EmptyStmt _) = st $ LitString "empty statement. for real."
--fromStatement (ES3.ExprStmt e) = fromExpression e
--fromStatement (
--fromStatement


declVar :: VarScope -> String -> Name -> VarScope
declVar scop name n = VarScope { parent = scop, vars = [(name, JSTVar n)] }

--printType :: InferredExpr -> IO ()
printType ex = do
  putStrLn $ "// " ++ (toJsDoc . fromJust . getExprType $ fst ex)
  putStrLn $ toJs . fst $ ex

ex expr = Expr expr ()
st expr = Expression $ ex expr

e1 = ex $ LitFunc ["arg"] ["vari"]
     $ [ st $ Var "vari"
       , st $ Assign (ex $ Var "vari") (ex $ LitObject [("amount", ex $ LitNumber 123)])
       , st $ Assign (ex $ Property (ex $ Var "vari") "amount") (ex $ LitNumber 0)
   --    , ex $ Assign (ex $ Var "vari") (ex $ LitString "ma?")
       , IfThenElse (ex $ LitBoolean False) (Return (ex $ LitArray [])) (Return $ ex $ LitArray [ex $ LitObject [("bazooka", ex $ Var "arg"), ("number", ex $ Var "vari")]])]
--e1 = ex $ LitFunc ["arg"] ["vari"] []

t1 = inferType Global e1
s1 = runState t1 emptyScope
s1doc = toJsDoc . fromJust . getExprType $ fst s1

e2 = ex $ Property (ex $ Index (ex $ Call e1 [(ex $ LitString "abc")]) (ex $ LitNumber 2)) "number"
s2 = runState (inferType Global e2) emptyScope


e3 = ex $ Assign (ex $ Var "f") e1
s3 = runState (inferType (declVar Global "f" 0) e3) emptyScope

