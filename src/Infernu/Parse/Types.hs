{-# LANGUAGE NoImplicitPrelude #-}
module Infernu.Parse.Types where

import qualified Infernu.Parse.Types.Grammar as Grammar
import           Infernu.Parse.Types.Grammar (TypeExp(..))
import qualified Infernu.Parse.Types.Tokens as Tokens
import           Infernu.Types ( TConsName(..)
                               , FType(..), TBody(..), Type
                               , TVarName(..))
import           Infernu.Fix (Fix(..))

import qualified Data.Char as Char

import           Infernu.Prelude

parseTypeExp :: String -> TypeExp
parseTypeExp s = Grammar.parseType (Tokens.scanTokens s)

toType :: TypeExp -> Type
--toType (App (Cons n) t2)
toType Number = Fix $ TBody TNumber
toType Boolean = Fix $ TBody TBoolean
toType String = Fix $ TBody TString
toType Regex = Fix $ TBody TRegex
toType Undefined = Fix $ TBody TUndefined
toType Null = Fix $ TBody TNull
-- toType EmptyThis = Fix $ TBody TEmptyThis
toType Date = Fix $ TBody TDate
toType (Array t) = Fix $ TCons TArray [toType t]
toType (Arr t1 t2) = Fix $ TFunc [toType t1] $ toType t2
toType (Var s) = Fix $ TBody (TVar (Flex n))
    where
        n = foldl (\n c -> (Char.ord c - Char.ord 'a') + 10 * n) 0 s
