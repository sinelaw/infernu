{-# LANGUAGE TupleSections #-}
module Infernu.Builtins.Regex
       (regexRowType)
       where

import           Control.Monad             (foldM, forM)
import Infernu.Types
import Infernu.InferState
import           Infernu.Lib (safeLookup)

func :: Type -> Type -> Type -> Type
func this x y = Fix $ TFunc [this, x] y

string :: Type
string = Fix $ TBody TString

regex :: Type
regex = Fix $ TBody TRegex

number :: Type
number = Fix $ TBody TNumber
        
ts :: t -> TScheme t
ts t = TScheme [] $ qualEmpty t

tvar :: TVarName -> Type
tvar = Fix . TBody . TVar
       
regexMatch :: Type
regexMatch = Fix . TRow (Just "RegexMatch")
             -- TODO: instead of quantifying 'this', it should be a recursive type (regexMatch itself)
             . TRowProp (TPropGetIndex) (TScheme [0] $ qualEmpty $ func (tvar 0) number string)
             . TRowProp (TPropName "index") (ts $ number)
             . TRowProp (TPropName "input") (ts $ string)
             $ TRowEnd Nothing

regexProps :: [(String, TypeScheme)]
regexProps = 
  [ ("source", ts string)
  , ("exec", ts $ func regex string regexMatch)
  ]

-- TODO: when inserting builtin types, do fresh renaming of scheme qvars
-- TODO: this code is actually pure, refactor to pure function and 'return' wrapper.
regexRowType :: Infer (TRowList Type)
regexRowType = foldM addProp (TRowEnd Nothing) $ regexProps
  where addProp rowlist (name, propTS) =
          do allocNames <- forM (schemeVars propTS) $ \tvName -> (fresh >>= return . (tvName,))
             let ts' = mapVarNames (safeLookup allocNames) propTS
             return $ TRowProp (TPropName name) ts' rowlist
