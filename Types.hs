{-# LANGUAGE DeriveGeneric, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

module Types where


import qualified Data.Map.Lazy as Map
import Data.Maybe(fromMaybe)
import Data.List((\\))
import Data.Traversable(Traversable(..))
import Data.Foldable(Foldable(..))
import Text.PrettyPrint.GenericPretty(Generic, Out(..))
import Prelude hiding (foldr, mapM)

type Name = Int

data Type a = TVar Name
            | TCons a [Type a]
               deriving (Show, Eq, Generic)

instance Out a => Out (Type a)

-- type signature = type scheme
-- page 172
data TypeSig a = TypeSig [Name] (Type a)
                 deriving (Show, Eq, Generic)
                        

type Subst a = Map.Map Name a

type TSubst a = Subst (Type a)

type TypeEnv a = Subst (TypeSig a)


substFromList :: [(Name, a)] -> Subst a
substFromList = Map.fromList

-- page 164
tvarsIn :: Type a -> [Name]
tvarsIn t = tvarsIn' t []
    where tvarsIn' (TVar x) names = x:names
          tvarsIn' (TCons _ ts) names = foldr tvarsIn' names ts

safeLookup :: Name -> TSubst a -> Type a
safeLookup name m = fromMaybe (TVar name) (Map.lookup name m)

-- page 166
substituteType :: TSubst a -> Type a -> Type a
substituteType m (TVar name) = safeLookup name m
substituteType m (TCons consName ts) = TCons consName $ map (substituteType m) ts


compose :: TSubst a -> TSubst a -> TSubst a
compose m2 m1 = Map.union merged m2
    where merged = Map.mapWithKey f m1
          f _ (TVar name') = safeLookup name' m2
          f _ t = t

extend :: Eq a => TSubst a -> Name -> Type a -> Either TypeError (TSubst a)
extend m name t
    | TVar name == t = Right m
    | name `elem` tvarsIn t = Left $ TypeError "occurs check failed"
    | otherwise = Right $ Map.insert name t m

unify :: Eq a => TSubst a -> Type a -> Type a -> Either TypeError (TSubst a)
unify m (TVar name) t = 
    if lookedUpType == TVar name
    then extend m name substType
    else unify m lookedUpType substType
    where lookedUpType = safeLookup name m
          substType = substituteType m t

unify m t1@(TCons _ _) t2@(TVar _) = unify m t2 t1
unify m (TCons consName1 ts1) (TCons consName2 ts2) =
    if consName1 == consName2
    then unifyl m (zip ts1 ts2)
    else Left  $ TypeError "type mismatch"

unifyl :: Eq a => TSubst a -> [(Type a, Type a)] -> Either TypeError (TSubst a)
unifyl m = foldr unify' (Right m)
    where unify' (t1, t2) (Right m') = unify m' t1 t2
          unify' _ (Left e) = Left e


freeVariables :: TypeSig a -> [Name]
freeVariables (TypeSig names t) = boundVars \\ names
    where boundVars = tvarsIn t

substitueTypeSig :: TSubst a -> TypeSig a -> TypeSig a
substitueTypeSig m (TypeSig names t) =
    TypeSig names $ substituteType (m `Map.difference` boundNames) t
    where boundNames = Map.fromList . zip names $ repeat ()

freeVariablesTE :: TypeEnv a -> [Name]
freeVariablesTE env = concat . map freeVariables . Map.elems $ env

substituteTE :: TSubst a -> TypeEnv a -> TypeEnv a
substituteTE tsubst env = Map.map (substitueTypeSig tsubst) env

-- page 178
newInstance :: [Name] -> TypeSig JSConsType -> Type JSConsType
newInstance nameSupply (TypeSig varNames t) = substituteType subst t
    where subst = substFromList . zip varNames $ map TVar nameSupply

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
               | LitFunc (Maybe String) [String] [Statement expr]
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
                    | VarDecl String
          deriving (Show, Eq, Generic, Functor, Foldable, Traversable)


data Expr a = Expr { exprBody :: Body (Expr a), exprData :: a }
          deriving (Show, Eq, Generic, Functor, Foldable, Traversable)


            
data TypeError = TypeError String 
                 deriving (Show, Eq, Generic)


data VarScope = Global | VarScope { parent :: VarScope, vars :: [(String, JSType)] }
               deriving (Show, Eq, Generic)


data TypeScope = TypeScope { tVars :: TSubst JSConsType
                           , maxNum :: Int
                           , tEnv :: TypeEnv JSConsType }
               deriving (Show, Eq, Generic)


data FuncScope = FuncScope { returnType :: JSType }
               deriving (Show, Eq, Generic)



data Scope = Scope { typeScope :: TypeScope
                   , varScope :: VarScope
                   , funcScope :: Maybe FuncScope }
               deriving (Show, Eq, Generic)

