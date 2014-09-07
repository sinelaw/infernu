
module Types where


import qualified Data.Map.Lazy as Map
import qualified Data.List as List 

type Name = Int

data Type = TVar Name
          | TCons String [Type]
            deriving (Eq, Show)

type TSubst = Map.Map Name Type

-- page 164
tvarsIn :: Type -> [Name]
tvarsIn t = tvarsIn' t []
    where tvarsIn' (TVar x) names = x:names
          tvarsIn' (TCons _ ts) names = foldr tvarsIn' names ts

safeLookup :: Name -> TSubst -> Type
safeLookup name m = case Map.lookup name m of
                      Nothing -> TVar name
                      Just t -> t

-- page 166
substituteType :: TSubst -> Type -> Type
substituteType m (TVar name) = safeLookup name m
substituteType m (TCons consName ts) = TCons consName $ map (substituteType m) ts


compose :: TSubst -> TSubst -> TSubst
compose m2 m1 = Map.union merged m2
    where merged = Map.mapWithKey f m1
          f _ (TVar name') = safeLookup name' m2
          f _ t = t


-- deltaSubst :: Name -> Type -> TSubst
-- deltaSubst name t

extend :: TSubst -> Name -> Type -> Maybe TSubst
extend m name t = if (TVar name) == t then Just m
                  else if name `elem` tvarsIn t then Nothing
                       else Just $ Map.insert name t m


unify :: TSubst -> Type -> Type -> Maybe TSubst
unify m (TVar name) t = 
    if lookedUpType == (TVar name)
    then extend m name substType
    else unify m lookedUpType substType
    where lookedUpType = safeLookup name m
          substType = substituteType m t

unify m t1@(TCons _ _) t2@(TVar _) = unify m t2 t1
unify m (TCons consName1 ts1) (TCons consName2 ts2) =
    if consName1 == consName2
    then unifyl m (zip ts1 ts2)
    else Nothing

unifyl :: TSubst -> [(Type, Type)] -> Maybe TSubst
unifyl m types = foldr unify' (Just m) types
    where unify' (t1, t2) (Just m') = unify m' t1 t2
          unify' _ Nothing = Nothing


-- type signature = type scheme
-- page 172
data TypeSig = TypeSig [Name] Type

freeVariables :: TypeSig -> [Name]
freeVariables (TypeSig names t) = (List.\\) boundVars names
    where boundVars = tvarsIn t

substitueTypeSig :: TSubst -> TypeSig -> TypeSig
substitueTypeSig m (TypeSig names t) =
    TypeSig names $ substituteType (m `Map.difference` boundNames) t
    where boundNames = Map.fromList . zip names $ repeat ()

