{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
-- |

module Infernu.Expr
       ( Exp(..)
       , mapTopAnnotation
       , LitVal(..)
       , EVarName(..), EPropName(..)
       , getAnnotations
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
            | LitRegex !String !Bool !Bool
            | LitUndefined
            | LitNull
            | LitEmptyThis
            deriving (Show, Eq, Ord)

data Exp a = EVar !a !EVarName
           | EApp !a !(Exp a) ![Exp a]
           | EAbs !a ![EVarName] !(Exp a)
           | ELet !a !EVarName !(Exp a) !(Exp a)
           | ECase !a !(Exp a) ![(LitVal, Exp a)]
           | EProp !a !(Exp a) !EPropName
           | EPropAssign !a !(Exp a) !EPropName !(Exp a) !(Exp a)
             -- TODO consider better options for causing rows to become closed outside the 'new' call
           | ENew !a !(Exp a) ![Exp a]
             -- Various literal expressions
           | ELit !a !LitVal
           | EArray !a ![Exp a]
           | ETuple !a ![Exp a]
           | ERow !a !Bool ![(EPropName, Exp a)]
           | EStringMap !a ![(String, Exp a)]
             deriving (Show, Eq, Ord, Functor, Foldable)

----------------------------------------------------------------------

-- TODO: Horrible, terrible boilerplate. get rid of it.
mapTopAnnotation :: (a -> a) -> Exp a -> Exp a
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

getAnnotations :: Exp a -> [a]
getAnnotations = foldr (:) []

