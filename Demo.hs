module Test where

import Control.Monad.State(runState)
import Data.Maybe(fromJust)

import Infer
import Pretty
import Types

-- ------------------------------------------------------------------------

ex expr = Expr expr ()

e1 = ex $ LitFunc ["arg"] ["vari"]
     $ [ex $ Var "vari"
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
