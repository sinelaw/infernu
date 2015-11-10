{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
-- |

{-# LANGUAGE NoImplicitPrelude #-}
module Infernu.Expr
       ( Exp(..)
       , mapTopAnnotation
       , LitVal(..)
       , EVarName(..), EPropName(..)
       ) where

import Data.Hashable (Hashable(..))
import GHC.Generics (Generic)

import           Infernu.Prelude


type EVarName = String
data EPropName = EPropName String
               | EPropGetIndex
               | EPropSetIndex
               | EPropFun
               deriving (Show, Eq, Ord, Generic)

instance Hashable EPropName where

data LitVal = LitNumber !Double
            | LitBoolean !Bool
            | LitString !String
            | LitRegex !String !Bool !Bool !Bool
            | LitUndefined
            | LitNull
            | LitEmptyThis
            deriving (Show, Eq, Ord)

data Exp t a = EVar !a !EVarName
             | EApp !a !(Exp t a) ![Exp t a]
             | EAbs !a ![EVarName] !(Exp t a)
             | ELet !a !EVarName !(Exp t a) !(Exp t a)
             | ECase !a !(Exp t a) ![(LitVal, Exp t a)]
             | EProp !a !(Exp t a) !EPropName
             | EPropAssign !a !(Exp t a) !EPropName !(Exp t a) !(Exp t a)
               -- TODO consider better options for causing rows to become closed outside the 'new' call
             | ENew !a !(Exp t a) ![Exp t a]
               -- Various literal expressions
             | ELit !a !LitVal
             | EArray !a ![Exp t a]
             | ETuple !a ![Exp t a]
             | ERow !a !Bool ![(EPropName, Exp t a)]
             | EStringMap !a ![(String, Exp t a)]
             | ETypeAscr !a !t !(Exp t a)
             deriving (Show, Eq, Ord, Functor, Foldable)

----------------------------------------------------------------------

-- TODO: Horrible, terrible boilerplate. get rid of it.
mapTopAnnotation :: (a -> a) -> Exp t a -> Exp t a
mapTopAnnotation f expr =
    case expr of
        (EVar a b) -> EVar (f a) b
        (EApp a x y) -> EApp (f a) x y
        (EAbs a x y) -> EAbs (f a) x y
        (ELet a x y z) -> ELet (f a) x y z
        (ELit a x) -> ELit (f a) x
        (EPropAssign a x y z v) -> EPropAssign (f a) x y z v
        (EArray a x) -> EArray (f a) x
        (ETuple a x) -> ETuple (f a) x
        (ERow a x y) -> ERow (f a) x y
        (EStringMap a x) -> EStringMap (f a) x
        (ECase a x ys) -> ECase (f a) x ys
        (EProp a x y) -> EProp (f a) x y
        (ENew a x y) -> ENew (f a) x y

----------------------------------------------------------------------
