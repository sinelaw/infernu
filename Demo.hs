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
fromStatement (ES3.BlockStmt _ stmts) = Block $ map fromStatement stmts
fromStatement (ES3.EmptyStmt _) = Empty
fromStatement (ES3.ExprStmt _ e) = Expression $ fromExpression e
fromStatement (ES3.IfStmt _ pred thenS elseS) = IfThenElse (fromExpression pred) (fromStatement thenS) (fromStatement elseS)
fromStatement (ES3.IfSingleStmt _ pred thenS) = IfThenElse (fromExpression pred) (fromStatement thenS) Empty
fromStatement (ES3.WhileStmt _ pred stmt) = While (fromExpression pred) (fromStatement stmt)
--fromStatement (ES3.ReturnStmt _ x) = maybe
--fromStatement (ES3.LabelledStmt _ _ s) = fromStatement s

--fromStatement

fromExpression :: ES3.Expression a -> Expr ()
fromExpression es3x = 
    ex $ case es3x of
      ES3.StringLit _ s -> LitString s
      ES3.RegexpLit _ s _ _ -> LitRegex s
      ES3.NumLit _ x -> LitNumber x
      ES3.IntLit _ x -> LitNumber $ fromIntegral x
      ES3.BoolLit _ x -> LitBoolean x
      ES3.ArrayLit _ xs -> LitArray $ map fromExpression xs
      ES3.ObjectLit _ props -> LitObject $ map (\(p, x) -> (fromProp p, fromExpression x)) props

fromProp :: ES3.Prop a -> String
fromProp (ES3.PropId _ (ES3.Id _ x)) = x
fromProp (ES3.PropString _ x) = x
fromProp (ES3.PropNum _ x) = show x

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
       , While (ex $ LitBoolean False) (st $ Assign (ex $ Property (ex $ Var "vari") "amount") (ex $ LitNumber 0))
   --    , ex $ Assign (ex $ Var "vari") (ex $ LitString "ma?")
       , IfThenElse (ex $ LitBoolean False) (Return $ Just . ex $ LitArray []) (Return $ Just . ex $ LitArray [ex $ LitObject [("bazooka", ex $ Var "arg"), ("number", ex $ Var "vari")]])]
--e1 = ex $ LitFunc ["arg"] ["vari"] []

t1 = inferType Global e1
s1 = runState t1 emptyScope
s1doc = toJsDoc . fromJust . getExprType $ fst s1

e2 = ex $ Property (ex $ Index (ex $ Call e1 [(ex $ LitString "abc")]) (ex $ LitNumber 2)) "number"
s2 = runState (inferType Global e2) emptyScope


e3 = ex $ Assign (ex $ Var "f") e1
s3 = runState (inferType (declVar Global "f" 0) e3) emptyScope

