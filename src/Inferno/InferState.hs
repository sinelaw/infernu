{-# LANGUAGE CPP             #-}
{-# LANGUAGE TupleSections   #-}
{-# LANGUAGE BangPatterns    #-}

module Inferno.InferState
       where

import           Control.Monad              (foldM, forM, forM_, liftM2)
import           Control.Monad.Trans        (lift)
import           Control.Monad.Trans.Either (EitherT (..), left, runEitherT)
import           Control.Monad.Trans.State  (StateT (..), evalStateT, get, modify)
import           Data.Foldable              (Foldable (..), msum)
import           Data.Traversable              (Traversable (..))
import           Data.Functor               ((<$>))
import           Data.Functor.Identity      (Identity (..), runIdentity)
import qualified Data.Map.Lazy              as Map
import           Data.Map.Lazy              (Map)
import           Data.Maybe                 (fromMaybe)
import qualified Data.Set                   as Set
import           Data.Set                   (Set)
import           Prelude                    hiding (foldr, sequence)
import qualified Text.Parsec.Pos            as Pos

import           Inferno.Pretty
import           Inferno.Types
import           Inferno.Log

-- | Inference monad. Used as a stateful context for generating fresh type variable names.
type Infer a = StateT InferState (EitherT TypeError Identity) a

runInferWith :: InferState -> Infer a -> Either TypeError a
runInferWith ns inf = runIdentity . runEitherT $ evalStateT inf ns

runInfer :: Infer a -> Either TypeError a
runInfer = runInferWith InferState { nameSource = NameSource { lastName = 0 }, mainSubst = nullSubst, varInstances = Map.empty, varSchemes = Map.empty, namedTypes = Map.empty }

fresh :: Infer TVarName
fresh = do
  modify $ \is -> is { nameSource = (nameSource is) { lastName = lastName (nameSource is) + 1 } }
  lastName . nameSource <$> get

freshVarId :: Infer VarId
freshVarId = VarId <$> fresh

throwError :: Pos.SourcePos -> String -> Infer a
throwError p s = lift . left $ TypeError p s

failWith :: Maybe a -> Infer a -> Infer a
failWith action err = case action of
                          Nothing -> err
                          Just x -> return x

failWithM :: Infer (Maybe a) -> Infer a -> Infer a
failWithM action err = do
  result <- action
  failWith result err

getVarSchemeByVarId :: VarId -> Infer (Maybe TypeScheme)
getVarSchemeByVarId varId = Map.lookup varId . varSchemes <$> get

getVarId :: EVarName -> TypeEnv -> Maybe VarId
getVarId = Map.lookup

getVarScheme :: Pos.SourcePos -> EVarName -> TypeEnv -> Infer (Maybe TypeScheme)
getVarScheme a n env = case getVarId n env of
                       Nothing -> throwError a $ "Unbound variable: '" ++ show n ++ "'"
                       Just varId -> getVarSchemeByVarId varId

setVarScheme :: EVarName -> VarId -> TypeScheme -> Infer ()
setVarScheme n varId scheme = do
  modify $ \is -> is { varSchemes = trace ("Inserting scheme for " ++ pretty n ++ ": " ++ pretty scheme) . Map.insert varId scheme $ varSchemes is }
  return ()

addVarScheme :: TypeEnv -> EVarName -> TypeScheme -> Infer TypeEnv
addVarScheme env n scheme = do
  varId <- tracePretty ("-- '" ++ pretty n ++ "' = varId") <$> freshVarId
  setVarScheme n varId scheme
  return $ Map.insert n varId env

----------------------------------------------------------------------
-- | Adds a pair of equivalent items to an equivalence map.
--
-- >>> let m1 = addEquivalence 1 2 Map.empty
-- >>> m1
-- fromList [(1,fromList [Fix (TBody (TVar 1)),Fix (TBody (TVar 2))]),(2,fromList [Fix (TBody (TVar 1)),Fix (TBody (TVar 2))])]
-- >>> addEquivalence 1 3 m1
-- fromList [(1,fromList [Fix (TBody (TVar 1)),Fix (TBody (TVar 2)),Fix (TBody (TVar 3))]),(2,fromList [Fix (TBody (TVar 1)),Fix (TBody (TVar 2)),Fix (TBody (TVar 3))]),(3,fromList [Fix (TBody (TVar 1)),Fix (TBody (TVar 2)),Fix (TBody (TVar 3))])]
-- >>> addEquivalence 3 1 m1
-- fromList [(1,fromList [Fix (TBody (TVar 1)),Fix (TBody (TVar 2)),Fix (TBody (TVar 3))]),(2,fromList [Fix (TBody (TVar 1)),Fix (TBody (TVar 2)),Fix (TBody (TVar 3))]),(3,fromList [Fix (TBody (TVar 1)),Fix (TBody (TVar 2)),Fix (TBody (TVar 3))])]
addEquivalence :: TVarName -> TVarName -> Map TVarName (Set (Type)) -> Map TVarName (Set (Type))
addEquivalence x y m = foldr (\k m' -> Map.insert k updatedSet m') m setTVars
    where updatedSet :: Set Type
          updatedSet = Set.insert (Fix $ TBody $ TVar x) . Set.insert (Fix $ TBody $ TVar y) $ Set.union (getSet x) (getSet y)
          getSet item = fromMaybe Set.empty $ Map.lookup item m
          setTVars :: [TVarName]
          setTVars = mapVarNames' $ Set.toList updatedSet
          mapVarNames' :: [Type] -> [TVarName]
          mapVarNames' [] = []
          mapVarNames' (Fix (TBody (TVar n)) : ts) = n : mapVarNames' ts
          mapVarNames' (_:ts) = mapVarNames' ts


addVarInstance :: TVarName -> TVarName -> Infer ()
addVarInstance x y = modify $ \is -> is { varInstances = tracePretty "updated equivs" $ addEquivalence x y (varInstances is) }

getFreeTVars :: TypeEnv -> Infer (Set TVarName)
getFreeTVars env = do
  let collectFreeTVs s varId = Set.union s <$> curFreeTVs
          where curFreeTVs = tr . maybe Set.empty freeTypeVars <$> getVarSchemeByVarId varId
                tr = tracePretty $ " collected from " ++ pretty varId ++ " free type variables: "
  foldM collectFreeTVs Set.empty (Map.elems env)

addNamedType :: TypeId -> Type -> TypeScheme -> Infer ()
addNamedType tid t scheme = do
  traceLog ("===> Introducing named type: " ++ pretty tid ++ " => " ++ pretty scheme) ()
  modify $ \is -> is { namedTypes = Map.insert tid (t, scheme) $ namedTypes is }
  return ()

-- | Compares schemes up to alpha equivalence including named type constructors equivalence (TCons
-- TName...).
-- 
-- >>> let mkNamedType tid ts = Fix $ TCons (TName (TypeId tid)) ts
--  
-- >>> areEquivalentNamedTypes (mkNamedType 0 [], TScheme [] (mkNamedType 0 [])) (mkNamedType 1 [], TScheme [] (mkNamedType 1 []))
-- True
-- >>> :{
--     areEquivalentNamedTypes (mkNamedType 0 [], TScheme [] (Fix $ TCons TFunc [Fix $ TBody TNumber, mkNamedType 0 []]))
--                             (mkNamedType 1 [], TScheme [] (Fix $ TCons TFunc [Fix $ TBody TNumber, mkNamedType 1 []]))
-- :}
-- True
--  
-- >>> :{
--     areEquivalentNamedTypes (mkNamedType 0 [Fix $ TBody $ TVar 10], TScheme [10] (Fix $ TCons TFunc [Fix $ TBody $ TVar 10, mkNamedType 0 [Fix $ TBody $ TVar 10]]))
--                             (mkNamedType 1 [Fix $ TBody $ TVar 11], TScheme [11] (Fix $ TCons TFunc [Fix $ TBody $ TVar 11, mkNamedType 1 [Fix $ TBody $ TVar 11]]))
-- :}
-- True
areEquivalentNamedTypes :: (Type, TypeScheme) -> (Type, TypeScheme) -> Bool
areEquivalentNamedTypes (t1, s1) (t2, s2) = s2 == (s2 { schemeType = applySubst subst $ replaceFix (unFix t1) (unFix t2) $ schemeType s1 })
  where subst = foldr (\(x,y) s -> singletonSubst x (Fix $ TBody $ TVar y) `composeSubst` s) nullSubst $ zip (schemeVars s1) (schemeVars s2)

-- Checks if a given type variable appears in the given type *only* as a parameter to a recursive
-- type name.  If yes, returns the name of recursive types (and position within) in which it
-- appears; otherwise returns Nothing.
isRecParamOnly :: TVarName -> Maybe (TypeId, Int) -> Type -> Maybe [(TypeId, Int)]
isRecParamOnly n1 typeId t1 =
  case unFix t1 of
   TBody (TVar n1') -> if n1' == n1 then sequence [typeId] else Just []
   TBody _ -> Just []
   TCons (TName typeId') subTs -> recurseIntoNamedType typeId' subTs
   TCons _ subTs -> msum $ map (isRecParamOnly n1 Nothing) subTs
   TRow rlist -> isRecParamRecList n1 rlist
     where isRecParamRecList n' rlist' =
             case rlist' of
              TRowEnd _ -> Just []
              TRowProp _ t' rlist'' -> liftM2 (++) (isRecParamOnly n1 Nothing t') (isRecParamRecList n' rlist'')
              TRowRec typeId' subTs -> recurseIntoNamedType typeId' subTs
  where recurseIntoNamedType typeId' subTs = msum $ map (\(t,i) -> isRecParamOnly n1 (Just (typeId', i)) t) $ zip subTs [0..]

dropAt :: Integral a => a -> [b] -> [b]
dropAt _ [] = []
dropAt 0 (_:xs) = xs
dropAt n (_:xs) = dropAt (n-1) xs

replaceRecType :: TypeId -> TypeId -> Int -> Type -> Type
replaceRecType typeId newTypeId indexToDrop t1 =
  case unFix t1 of
   TBody _ -> t1
   TCons (TName typeId') subTs -> if typeId == typeId'
                                  then Fix $ TCons (TName newTypeId) $ dropAt indexToDrop subTs
                                  else t1
   TCons n subTs -> Fix $ TCons n $ map (replaceRecType typeId newTypeId indexToDrop) subTs
   TRow rlist -> Fix $ TRow $ go rlist
     where go rlist' =
             case rlist' of
              TRowEnd _ -> rlist'
              TRowProp p t' rlist'' -> TRowProp p (replaceRecType typeId newTypeId indexToDrop t') (go rlist'')
              TRowRec tid ts -> if typeId == tid
                                then TRowRec newTypeId $ dropAt indexToDrop ts
                                else rlist'

allocNamedType :: TVarName -> Type -> Infer Type
allocNamedType n t =
  do typeId <- TypeId <$> fresh
     let namedType = TCons (TName typeId) $ map (Fix . TBody . TVar) $ Set.toList $ freeTypeVars t `Set.difference` Set.singleton n
         target = replaceFix (TBody (TVar n)) namedType t
     scheme <- generalize Map.empty target
     currentNamedTypes <- filter (areEquivalentNamedTypes (Fix namedType, scheme)) . map snd . Map.toList . namedTypes <$> get
     case currentNamedTypes of
      [] -> do addNamedType typeId (Fix namedType) scheme
               return $ Fix namedType
      (otherNT, _):_ -> return otherNT

resolveSimpleMutualRecursion :: TVarName -> Type -> TypeId -> Int -> Infer Type
resolveSimpleMutualRecursion n t tid ix =
  do (Fix (TCons (TName _) ts), scheme) <- (Map.lookup tid . namedTypes <$> get) `failWithM` error "oh no." -- TODO
     newTypeId <- TypeId <$> fresh
     let qVars' = dropAt ix $ schemeVars scheme
         replaceOldNamedType = replaceRecType tid newTypeId ix
         sType' = replaceOldNamedType $ schemeType scheme
         newTs = dropAt ix $ ts
         newNamedType = Fix (TCons (TName newTypeId) newTs)
         --updatedNamedType = Fix (TCons (TName tid) newTs)
         updatedScheme = applySubst (singletonSubst n newNamedType) $ TScheme qVars' sType'
         
     addNamedType newTypeId newNamedType updatedScheme
     -- TODO: we could alternatively update the existing named type, but that will break it's schema (will now take less params?)
     --addNamedType tid updatedNamedType updatedScheme
     return $ replaceOldNamedType t
     
     
getNamedType :: TVarName -> Type -> Infer Type
getNamedType n t =
  do let recTypeParamPos = isRecParamOnly n Nothing t
     traceLog ("isRecParamOnly: " ++ pretty n ++ " in " ++ pretty t ++ ": " ++ (show $ fmap pretty $ recTypeParamPos)) ()
     case recTypeParamPos of
      Just [(tid, ix)] -> resolveSimpleMutualRecursion n t tid ix
      -- either the variable appears outside a recursive type's type parameter list, or it appears
      -- in more than one such position:
      _ -> allocNamedType n t 


unrollNameByScheme :: [Type] -> [TVarName] -> Type -> Type
unrollNameByScheme ts qvars t = applySubst subst t
  where assocs = zip qvars ts
        subst = foldr (\(tvar,destType) s -> singletonSubst tvar destType `composeSubst` s) nullSubst assocs

-- | Unrolls (expands) a TName recursive type by plugging in the holes from the given list of types.
-- Similar to instantiation, but uses a pre-defined set of type instances instead of using fresh
-- type variables.
unrollName :: Pos.SourcePos -> TypeId -> [Type] -> Infer Type
unrollName a tid ts =
  do (TScheme qvars t) <- (fmap snd . Map.lookup tid . namedTypes <$> get) `failWithM` throwError a "Unknown type id"
     return $ unrollNameByScheme ts qvars t
-- | Applies a subsitution onto the state (basically on the variable -> scheme map).
--
-- >>> :{
-- runInfer $ do
--     let t = TScheme [0] (Fix $ TCons TFunc [Fix $ TBody (TVar 0), Fix $ TBody (TVar 1)])
--     let tenv = Map.empty
--     tenv' <- addVarScheme tenv "x" t
--     applySubstInfer $ Map.singleton 0 (Fix $ TBody TString)
--     varSchemes <$> get
-- :}
-- Right (fromList [(VarId 1,TScheme {schemeVars = [0], schemeType = Fix (TCons TFunc [Fix (TBody TString),Fix (TBody (TVar 1))])})])
--
applySubstInfer :: TSubst -> Infer ()
applySubstInfer s =
  do traceLog ("applying subst: " ++ pretty s) ()
     modify $ applySubst s

-- | Instantiate a type scheme by giving fresh names to all quantified type variables.
--
-- For example:
--
-- >>> runInferWith (InferState { nameSource = NameSource 2, mainSubst = Map.empty, varInstances = Map.empty, varSchemes = Map.empty, namedTypes = Map.empty }) . instantiate $ TScheme [0] (Fix $ TCons TFunc [Fix $ TBody (TVar 0), Fix $ TBody (TVar 1)])
-- Right Fix (TCons TFunc [Fix (TBody (TVar 3)),Fix (TBody (TVar 1))])
--
-- In the above example, type variable 0 has been replaced with a fresh one (3), while the unqualified free type variable 1 has been left as-is.
--
-- >>> :{
-- runInfer $ do
--     let t = TScheme [0] (Fix $ TCons TFunc [Fix $ TBody (TVar 0), Fix $ TBody (TVar 1)])
--     let tenv = Map.empty
--     tenv' <- addVarScheme tenv "x" t
--     instantiateVar (Pos.initialPos "") "x" tenv'
-- :}
-- Right Fix (TCons TFunc [Fix (TBody (TVar 2)),Fix (TBody (TVar 1))])
--
instantiate :: TypeScheme -> Infer (Type)
instantiate (TScheme tvarNames t) = do
  allocNames <- forM tvarNames $ \tvName -> do
    freshName <- fresh
    return (tvName, freshName)
  forM_ allocNames $ uncurry addVarInstance
  let replaceVar n = fromMaybe n $ lookup n allocNames
  return $ mapVarNames replaceVar t

instantiateVar :: Pos.SourcePos -> EVarName -> TypeEnv -> Infer (Type)
instantiateVar a n env = do
  varId <- getVarId n env `failWith` throwError a ("Unbound variable: '" ++ show n ++ "'")
  scheme <- getVarSchemeByVarId varId `failWithM` throwError a ("Assertion failed: missing var scheme for: '" ++ show n ++ "'")
  tracePretty ("Instantiated var '" ++ pretty n ++ "' with scheme: " ++ pretty scheme ++ " to") <$> instantiate scheme

----------------------------------------------------------------------
-- | Generalizes a type to a type scheme, i.e. wraps it in a "forall" that quantifies over all
--   type variables that are free in the given type, but are not free in the type environment.
--
-- Example:
--
-- >>> runInfer $ generalize Map.empty $ Fix $ TCons TFunc [Fix $ TBody (TVar 0),Fix $ TBody (TVar 1)]
-- Right (TScheme {schemeVars = [0,1], schemeType = Fix (TCons TFunc [Fix (TBody (TVar 0)),Fix (TBody (TVar 1))])})
--
-- >>> :{
-- runInfer $ do
--     let t = TScheme [1] (Fix $ TCons TFunc [Fix $ TBody (TVar 0), Fix $ TBody (TVar 1)])
--     tenv <- addVarScheme Map.empty "x" t
--     generalize tenv (Fix $ TCons TFunc [Fix $ TBody (TVar 0), Fix $ TBody (TVar 2)])
-- :}
-- Right (TScheme {schemeVars = [2], schemeType = Fix (TCons TFunc [Fix (TBody (TVar 0)),Fix (TBody (TVar 2))])})
--
-- In this example the steps were:
--
-- 1. Environment: { x :: forall 0. 0 -> 1 }
--
-- 2. generalize (1 -> 2)
--
-- 3. result: forall 2. 1 -> 2
--
-- >>> runInfer $ generalize Map.empty (Fix $ TCons TFunc [Fix $ TBody (TVar 0), Fix $ TBody (TVar 0)])
-- Right (TScheme {schemeVars = [0], schemeType = Fix (TCons TFunc [Fix (TBody (TVar 0)),Fix (TBody (TVar 0))])})
--
-- TODO add tests for monotypes
generalize :: TypeEnv -> Type -> Infer TypeScheme
generalize tenv t = do
  s <- getMainSubst
  let t' = applySubst s t
  unboundVars <- Set.difference (freeTypeVars t') <$> getFreeTVars tenv
  return $ TScheme (Set.toList unboundVars) t'

minifyVarsFunc :: (VarNames a) => a -> TVarName -> TVarName
minifyVarsFunc xs n = fromMaybe n $ Map.lookup n vars
  where vars = Map.fromList $ zip (Set.toList $ freeTypeVars xs) ([0..] :: [TVarName])

minifyVars :: (VarNames a) => a -> a
minifyVars xs = mapVarNames (minifyVarsFunc xs) xs

getVarInstances :: Infer (Map.Map TVarName (Set.Set (Type)))
getVarInstances = varInstances <$> get


getMainSubst :: Infer TSubst
getMainSubst = mainSubst <$> get

applyMainSubst :: Substable b => b -> Infer b
applyMainSubst x =
  do s <- getMainSubst
     return $ applySubst s x
