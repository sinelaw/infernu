module Test where

import Text.PrettyPrint.GenericPretty(pp)
import Control.Monad.State(runState)
import Data.Maybe(fromJust)

import Infer
import Pretty
import Types

-- ------------------------------------------------------------------------
--data VarScope = Global | VarScope { parent :: VarScope, vars :: [(String, JSType)] }

declVar :: VarScope -> String -> Name -> VarScope
declVar scop name n = VarScope { parent = scop, vars = [(name, JSTVar n)] }

--printType :: InferredExpr -> IO ()
printType ex = do
  putStrLn $ "// " ++ (toJsDoc . fromJust . getExprType $ fst ex)
  putStrLn $ toJs . fst $ ex

ex expr = Expr expr ()

e1 = ex $ LitFunc ["arg"] ["vari"]
     $ [ ex $ Var "vari"
       , ex $ Assign (ex $ Var "vari") (ex $ LitObject [("amount", ex $ LitNumber 123)])
       , ex $ Assign (ex $ Property (ex $ Var "vari") "amount") (ex $ LitNumber 0)
   --    , ex $ Assign (ex $ Var "vari") (ex $ LitString "ma?")
       , ex $ Return (ex $ LitArray [])
       , ex $ Return (ex $ LitArray [ex $ LitObject [("bazooka", ex $ Var "arg"), ("number", ex $ Var "vari")]])]
--e1 = ex $ LitFunc ["arg"] ["vari"] []

t1 = inferType Global e1
s1 = runState t1 emptyScope
s1doc = toJsDoc . fromJust . getExprType $ fst s1

e2 = ex $ Property (ex $ Index (ex $ Call e1 [(ex $ LitString "abc")]) (ex $ LitNumber 2)) "number"
s2 = runState (inferType Global e2) emptyScope


e3 = ex $ Assign (ex $ Var "f") e1
s3 = runState (inferType (declVar Global "f" 0) e3) emptyScope

