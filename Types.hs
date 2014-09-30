{-# LANGUAGE DeriveGeneric, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Types where

import qualified Data.Map.Lazy as Map
import Data.Maybe(fromMaybe)
import Data.List((\\))
import Data.Traversable(Traversable(..))
import Data.Foldable(Foldable(..))
import Text.PrettyPrint.GenericPretty(Generic, Out(..))
import Prelude hiding (foldr, mapM)

-----------------------------------------------------------------------
import Debug.Trace

assertEqual :: (Show a, Eq a) => a -> a -> Bool
assertEqual a b = if a == b then True else trace ("    expected: " ++ show b ++ "\n    got:      " ++ show a) False
traceTypes s x = x --traceShow (s ++ show x) x

-----------------------------------------------------------------------
 
type Name = Int

data Type a = TVar Name
            | TCons a [Type a]
               deriving (Show, Eq, Generic)

instance Out a => Out (Type a)

-- type signature = type scheme
-- = forall (vars) . type
-- page 172
data TypeSig a = TypeSig [Name] (Type a)
                 deriving (Show, Eq, Generic)
                        
--instance (Out k, Out v) => Out (Map.Map k v)

type Subst a = Map.Map Name a

-- TSubst = map tvarname -> type
-- e.g. "a" = Bool -> Int
type TSubst a = Subst (Type a)

-- TypeEnv = map (value variable name) -> type sig
-- e.g. "x" :: forall "a" "b". "a" -> "b" -> Bool
type TypeEnv a = Map.Map String (TypeSig a)

idSubst :: Subst a 
idSubst = Map.empty

emptyTEnv :: TypeEnv a
emptyTEnv = Map.empty

substFromList :: [(Name, a)] -> Subst a
substFromList = Map.fromList

-- page 164
tvarsIn :: Type a -> [Name]
tvarsIn t = tvarsIn' [] t
    where tvarsIn' names (TVar x) = x:names
          tvarsIn' names (TCons _ ts) = foldl' tvarsIn' names ts

safeLookup :: Name -> TSubst a -> Type a
safeLookup name m = fromMaybe (TVar name) (Map.lookup name m)


-- page 166
substituteType :: TSubst a -> Type a -> Type a
substituteType m (TVar name) = safeLookup name m
substituteType m (TCons consName ts) = TCons consName $ map (substituteType m) ts


prop_substituteType_identity :: Type JSConsType -> Bool
prop_substituteType_identity t = substituteType idSubst t == t

prop_substituteType_basic :: () -> Bool
prop_substituteType_basic _ = assertEqual (substituteType subst t) expectedType
    where subst = Map.fromList [(1, toType u)]
          t = toType $ JSFunc [JSTVar 1] (JSTVar 1)
          u = JSFunc [JSTVar 2] (JSTVar 3)
          expectedType = toType $ JSFunc [u] (u)

-- | Compose substitutions. [[ m1 `compose` m2 ]] = [[ m1 ]] . [[ m2 ]] 
-- | where the TSubst values are seen as functions (for type variable names to types)
compose :: TSubst a -> TSubst a -> TSubst a
compose m1 m2 = (Map.map (substituteType m1) m2) `Map.union` m1


prop_compose_distributive :: TSubst JSConsType -> TSubst JSConsType -> Type JSConsType -> Bool
prop_compose_distributive s1 s2 t = substituteType (compose s1 s2) t == (substituteType s1 . substituteType s2 $ t)

-- | Adds a substitution from type variable name to type, to the given substitution.
-- | The given pair must not be recursive, i.e. the type variable name must not appear in the given type, otherwise an occurs check will fail.
extend :: Eq a => TSubst a -> Name -> Type a -> Either (TypeError a) (TSubst a)
extend m name t
    | TVar name == t = Right m
    | name `elem` tvarsIn t = Left $ OccursCheckError name t 
    | otherwise = Right $ Map.insert name t m


prop_extend :: TSubst JSConsType -> Name -> Type JSConsType -> Bool
prop_extend subst n t = case subst' of
                          Right subst'' -> substituteType subst'' (TVar n) == t
                          Left _ -> True -- TODO verify infinite type
    where subst' = (extend subst n t)


-- | Subst is assumed to not have cycles
unify :: Eq a => TSubst a -> Type a -> Type a -> Either (TypeError a) (TSubst a)
unify m (TVar name) t = 
    if lookedUpType == TVar name
    then extend m name substType
    else unify m lookedUpType substType
    where lookedUpType = safeLookup name m
          substType = substituteType m t
unify m t1@(TCons _ _) t2@(TVar _) = unify m t2 t1
unify m t1@(TCons consName1 ts1) t2@(TCons consName2 ts2) =
    if (consName1 == consName2) && (length ts1 == length ts2)
    then unifyl m (zip ts1 ts2)
    else Left $ TypeMismatch "TCons names do not match" t1 t2

unifyl :: Eq a => TSubst a -> [(Type a, Type a)] -> Either (TypeError a) (TSubst a)
unifyl m = foldr unify'' (Right m)
    where unify'' (t1, t2) (Right m') = unify m' t1 t2
          unify'' _ (Left e) = Left e

prop_unify_self :: TSubst JSConsType -> Type JSConsType -> Bool
prop_unify_self subst t = case unify subst t t of
                            Left _ -> False
                            Right _ -> True

-- | Finds the free vars in a type signature, i.e. var names used in the type but not mentioned in the sig's bound names
freeVariables :: TypeSig a -> [Name]
freeVariables (TypeSig names t) = boundVars \\ names
    where boundVars = tvarsIn t

-- | Performs substitution on a type signature, but ignoring the bound var names
substitueTypeSig :: TSubst a -> TypeSig a -> TypeSig a
substitueTypeSig m (TypeSig names t) =
    TypeSig names $ substituteType (m `Map.difference` boundNames) t
    where boundNames = Map.fromList . zip names $ repeat ()

freeVariablesTE :: TypeEnv a -> [Name]
freeVariablesTE env = concat . map freeVariables . Map.elems $ env

-- | Updates a type environment's type signatures using the given type substitution
substituteTE :: TSubst a -> TypeEnv a -> TypeEnv a
substituteTE tsubst env = Map.map (substitueTypeSig tsubst) env

setTypeSig :: String -> TypeSig a -> TypeEnv a -> TypeEnv a
setTypeSig = Map.insert

generalize :: TypeEnv a -> Type a -> TypeSig a
generalize tenv t = TypeSig (tvarsIn t \\ freeVariablesTE tenv) t

--------------------------------------------------------------------

data JSConsType = JSConsBoolean | JSConsNumber | JSConsString | JSConsRegex
                | JSConsFunc
                | JSConsArray
                | JSConsObject [String]
                | JSConsUndefined
               deriving (Show, Eq, Generic)

instance Out JSConsType

data JSType = JSBoolean | JSNumber | JSString | JSRegex
            | JSFunc [JSType] JSType
            | JSArray JSType
            | JSObject [(String, JSType)]
            | JSTVar Name
            | JSUndefined
               deriving (Show, Eq, Generic)

instance Out JSType


getObjPropertyType :: JSType -> String -> Maybe JSType
getObjPropertyType (JSObject props) name = lookup name props
getObjPropertyType _ _ = Nothing

toType :: JSType -> Type JSConsType
toType (JSTVar name) = TVar name
toType JSBoolean = TCons JSConsBoolean []
toType JSNumber = TCons JSConsNumber []
toType JSString = TCons JSConsString []
toType JSRegex = TCons JSConsRegex []
toType (JSFunc argsT resT) = TCons JSConsFunc $ toType resT : map toType argsT
toType (JSArray elemT) = TCons JSConsArray [toType elemT]
toType (JSObject propsT) = TCons (JSConsObject $ map fst propsT) 
                           $ map (toType . snd) propsT
toType JSUndefined = TCons JSConsUndefined []


fromType :: Type JSConsType -> JSType
fromType (TVar name) = JSTVar name
fromType (TCons consName types) =
    case consName of
      JSConsBoolean -> JSBoolean
      JSConsNumber -> JSNumber
      JSConsString -> JSString
      JSConsRegex -> JSRegex
      JSConsUndefined -> JSUndefined
      JSConsFunc -> JSFunc (map fromType . tail $ types) (fromType . head $ types)
      JSConsArray -> JSArray (fromType . head $ types) -- TODO ensure single
      JSConsObject names -> JSObject $ zip names (map fromType types)

--data LValue = Var String | StrIndex Expr String | NumIndex Expr Int
data Body expr = LitBoolean Bool 
               | LitNumber Double 
               | LitString String 
               | LitRegex String 
               | Var String
               | LitFunc { litFuncName :: Maybe String
                         , litFuncArgs :: [String]
                         , litFuncVars :: [String]
                         , litFuncBody :: Statement expr
                         }
               | LitArray [expr] 
               | LitObject [(String, expr)]
               | Call expr [expr]
               | Assign expr expr -- lvalue must be a property (could represent a variable)
               | Property expr String  -- lvalue must be a JSObject
               | Index expr expr  -- lvalue must be a JArray
--               | Return expr
          deriving (Show, Eq, Generic, Functor, Foldable, Traversable)

data Statement expr = Empty
                    | Expression expr
                    | Block [Statement expr] 
                    | IfThenElse expr (Statement expr) (Statement expr)
                    | While expr (Statement expr)
                    | Return (Maybe expr)
          deriving (Show, Eq, Generic, Functor, Foldable, Traversable)


data Expr a = Expr { exprBody :: Body (Expr a), exprData :: a }
          deriving (Show, Eq, Generic, Functor, Foldable, Traversable)


            
data TypeError a = GenericTypeError String 
                 | OccursCheckError Name (Type a)
                 | TypeMismatch String (Type a) (Type a)
                 deriving (Show, Eq, Generic)


-- data VarScope = Global | VarScope { parent :: VarScope, vars :: [(String, JSType)] }
--                deriving (Show, Eq, Generic)


-- data TypeScope = TypeScope { tVars :: TSubst JSConsType
--                            , maxNum :: Int
--                            , tEnv :: TypeEnv JSConsType }
--                deriving (Show, Eq, Generic)


-- data FuncScope = FuncScope { returnType :: JSType }
--                deriving (Show, Eq, Generic)



-- data Scope = Scope { typeScope :: TypeScope
--                    , varScope :: VarScope
--                    , funcScope :: Maybe FuncScope }
--                deriving (Show, Eq, Generic)

