{-# LANGUAGE TupleSections #-}
module Infernu.Builtins.String
       (stringRowType)
       where

import           Control.Monad             (foldM, forM)
import Infernu.Types
import Infernu.InferState
import           Infernu.Lib (safeLookup)

func :: Type -> Type -> Type -> Type
func this x y = Fix $ TFunc [this, x] y

funcN :: [Fix FType] -> Fix FType -> Fix FType
funcN xs tres = Fix $ TFunc xs tres

string :: Type
string = Fix $ TBody TString

number :: Type
number = Fix $ TBody TNumber
        
ts :: t -> TScheme t
ts t = TScheme [] $ qualEmpty t

tvar :: TVarName -> Type
tvar = Fix . TBody . TVar

withTypeClass n t t' = TQual { qualPred = [TPredIsIn { predClass = ClassName n, predType = t }], qualType = t' }

stringProps :: [(String, TypeScheme)]
stringProps = 
  [ ("length", ts number)
  , ("charAt", ts $ func string number string)
  , ("charCodeAt", ts $ func string number number)
  , ("concat", ts $ func string string string) -- TODO: concat really accepts a variable number of arguments
  , ("indexOf", ts $ func string string number) -- TODO: optional parameter
  , ("lastIndexOf", ts $ func string string number) -- TODO: optional parameter
  , ("localeCompare", ts $ func string string number) -- TODO: optional parameters

-- To support 'match' we need to allow different result types, different for global and non-global
-- regexes.  One possibility is to define two regex types RegexSingle and RegexGlobal and use
-- associated types:
-- class Regex r where
--     type R r = r
--     type M RegexLocal = -- match result type for RegexLocal
--     type M RegexGlobal = [String]
-- 
--  , ("match", ts $ func string regex

  , ("replace", TScheme [0] $ withTypeClass "Pattern" (tvar 0) $ funcN [string, tvar 0, string] string)
  ]

-- TODO: when inserting builtin types, do fresh renaming of scheme qvars
-- TODO: this code is actually pure, refactor to pure function and 'return' wrapper.
stringRowType :: Infer (TRowList Type)
stringRowType = TRowProp TPropGetIndex (ts $ func string number string) <$> namedProps
  where namedProps = foldM addProp (TRowEnd Nothing) $ stringProps
        addProp rowlist (name, propTS) =
          do allocNames <- forM (schemeVars propTS) $ \tvName -> (fresh >>= return . (tvName,))
             let ts' = mapVarNames (safeLookup allocNames) propTS
             return $ TRowProp (TPropName name) ts' rowlist
