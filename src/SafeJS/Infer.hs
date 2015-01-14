{-# LANGUAGE CPP             #-}
{-# LANGUAGE TupleSections   #-}
{-# LANGUAGE BangPatterns    #-}

module SafeJS.Infer
    ( runTypeInference
    , test
    , Pretty(..)
    , pretty
    , getAnnotations
    , minifyVars
    , TypeError
#ifdef QUICKCHECK
    , runAllTests
#endif
    )
    where

import           Data.Monoid                (Monoid(..))
import           Control.Monad              (foldM, forM, forM_, liftM2)
--import           Control.Monad.State (State, evalState, get, modify)
import           Control.Monad.Trans        (lift)
-- Use Control.Monad.Trans.Except
import           Control.Monad.Trans.Either (EitherT (..), left, runEitherT)
import           Control.Monad.Trans.State  (StateT (..), evalStateT, get,
                                             modify)
import           Data.Foldable              (Foldable (..), msum)
import           Data.Traversable              (Traversable (..))
import           Data.Functor               ((<$>))
import           Data.Functor.Identity      (Identity (..), runIdentity)
import qualified Data.Map.Lazy              as Map
import           Data.Map.Lazy              (Map)
import           Data.Maybe                 (fromMaybe, mapMaybe)
import qualified Data.Set                   as Set
import           Data.Set                   (Set)
import           Prelude                    hiding (foldr, sequence)
import qualified Text.Parsec.Pos            as Pos

#ifdef QUICKCHECK
import           Data.DeriveTH
import           Test.QuickCheck            (choose, resize)
import           Test.QuickCheck.All
import           Test.QuickCheck.Arbitrary  (Arbitrary (..))
#endif

import           SafeJS.Pretty
import           SafeJS.Types
import qualified SafeJS.Builtins            as Builtins
import           SafeJS.Decycle

#if TRACE
import           Debug.Trace                (trace)
#else
trace :: a -> b -> b
trace _ y = y
#endif

tracePretty :: Pretty a => String -> a -> a
tracePretty prefix x = trace (prefix ++ " " ++ pretty x) x

traceLog :: Monad m => String -> a -> m a
traceLog !s !r = return $! trace s r `seq` r

----------------------------------------------------------------------

-- var x = 2;    --> let x = ref 2 in    | x :: a
-- x = 3;        -->   x := 3            |

-- var f = function (x) { return [x]; }    --> let f = ref (\x -> arr [x])  :: Ref (forall a. a -> [a])
-- var g = f;                              -->     g = ref (!f)             :: Ref (forall a. a -> [a])
-- var st = f('abc');                      -->     st = ref (!f 'abc')      :: Ref [String]
-- var num = f(1234);                      -->     num = ref (!f 1234)      :: Ref [Number]

----------------------------------------------------------------------


-- instance (Functor f, Foldable f, Types a) => Types (f a) where
--   freeTypeVars = foldr (Set.union . freeTypeVars) Set.empty
--   applySubst s = fmap (applySubst s)
----------------------------------------------------------------------

ungeneralize :: TScheme -> TScheme
ungeneralize (TScheme _ tbody) = TScheme [] tbody

getQuantificands :: TScheme -> [TVarName]
getQuantificands (TScheme tvars _) = tvars

getAnnotations :: Exp a -> [a]
getAnnotations = foldr (:) []


-- alphaEquivalent :: TScheme -> TScheme -> Bool
-- alphaEquivalent ts1@(TScheme tvn1 _) (TScheme tvn2 t2) = ts1 == TScheme tvn1 ts2'
--     where TScheme _ ts2' = applySubst substVarNames (TScheme [] t2)
--           substVarNames = Map.fromList . map (\(old,new) -> (old, TBody $ TVar new)) $ zip tvn2 tvn1

----------------------------------------------------------------------

nullSubst :: TSubst
nullSubst = Map.empty

-- | composeSubst should obey the law:
-- applySubst (composeSubst new old) t = applySubst new (applySubst old t)
-- >>> composeSubst (Map.fromList []) (Map.fromList [])
-- fromList []
-- >>> composeSubst (Map.fromList [(0,Fix (TBody (TVar 1)))]) (Map.fromList [])
-- fromList [(0,Fix (TBody (TVar 1)))]
-- >>> composeSubst (Map.fromList []) (Map.fromList [(0,Fix (TBody (TVar 1)))])
-- fromList [(0,Fix (TBody (TVar 1)))]
-- >>> composeSubst (Map.fromList [(1,Fix (TBody (TVar 2)))]) (Map.fromList [(0,Fix (TBody (TVar 1)))])
-- fromList [(0,Fix (TBody (TVar 2))),(1,Fix (TBody (TVar 2)))]
-- >>> composeSubst (Map.fromList [(0,Fix (TBody (TVar 1)))]) (Map.fromList [(1,Fix (TBody (TVar 2)))])
-- fromList [(0,Fix (TBody (TVar 2))),(1,Fix (TBody (TVar 2)))]
composeSubst :: TSubst -> TSubst -> TSubst
composeSubst new old = applySubst new old `Map.union` new

singletonSubst :: TVarName -> Type -> TSubst
singletonSubst = Map.singleton

#ifdef QUICKCHECK
prop_composeSubst :: TSubst -> TSubst -> Type -> Bool
prop_composeSubst new old t = applySubst (composeSubst new old) t == applySubst new (applySubst old t)
#endif
----------------------------------------------------------------------

getVarId :: EVarName -> TypeEnv -> Maybe VarId
getVarId = Map.lookup

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


-- | Inference monad. Used as a stateful context for generating fresh type variable names.
type Infer a = StateT InferState (EitherT TypeError Identity) a

runInferWith :: InferState -> Infer a -> Either TypeError a
runInferWith ns inf = runIdentity . runEitherT $ evalStateT inf ns

runInfer :: Infer a -> Either TypeError a
runInfer = runInferWith InferState { nameSource = NameSource { lastName = 0 }, varInstances = Map.empty, varSchemes = Map.empty, namedTypes = Map.empty }

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

getVarSchemeByVarId :: VarId -> Infer (Maybe TScheme)
getVarSchemeByVarId varId = Map.lookup varId . varSchemes <$> get

getVarScheme :: Pos.SourcePos -> EVarName -> TypeEnv -> Infer (Maybe TScheme)
getVarScheme a n env = case getVarId n env of
                       Nothing -> throwError a $ "Unbound variable: '" ++ show n ++ "'"
                       Just varId -> getVarSchemeByVarId varId

setVarScheme :: EVarName -> VarId -> TScheme -> Infer ()
setVarScheme n varId scheme = do
  modify $ \is -> is { varSchemes = trace ("Inserting scheme for " ++ pretty n ++ ": " ++ pretty scheme) . Map.insert varId scheme $ varSchemes is }
  return ()

addVarScheme :: TypeEnv -> EVarName -> TScheme -> Infer TypeEnv
addVarScheme env n scheme = do
  varId <- tracePretty ("-- '" ++ pretty n ++ "' = varId") <$> freshVarId
  setVarScheme n varId scheme
  return $ Map.insert n varId env

addVarInstance :: TVarName -> TVarName -> Infer ()
addVarInstance x y = modify $ \is -> is { varInstances = tracePretty "updated equivs" $ addEquivalence x y (varInstances is) }

getFreeTVars :: TypeEnv -> Infer (Set TVarName)
getFreeTVars env = do
  let collectFreeTVs s varId = Set.union s <$> curFreeTVs
          where curFreeTVs = tr . maybe Set.empty freeTypeVars <$> getVarSchemeByVarId varId
                tr = tracePretty $ " collected from " ++ pretty varId ++ " free type variables: "
  foldM collectFreeTVs Set.empty (Map.elems env)

addNamedType :: TypeId -> Type -> TScheme -> Infer ()
addNamedType tid t scheme = do
  traceLog ("===> Introducing named type: " ++ pretty tid ++ " => " ++ pretty scheme) ()
  modify $ \is -> is { namedTypes = Map.insert tid (t, scheme) $ namedTypes is }
  return ()

-- Should ignore typeids and compare only schemes up to alpha equivalence including named type
-- constructors equivalence (TCons TName...).
areEquivalentNamedTypes :: (TypeId, (Type, TScheme)) -> (TypeId, (Type, TScheme)) -> Bool
areEquivalentNamedTypes (_, (t1, s1)) (_, (t2, s2)) = s2 == (TScheme { schemeVars = schemeVars s1', schemeType = replaceFix (unFix t1) (unFix t2) (schemeType s1') })
  where s1' = mapVarNames (safeLookup' $ zip (schemeVars s1) (schemeVars s2)) s1
        safeLookup' abs' a = case lookup a abs' of
          Nothing -> a
          Just b -> b

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
     currentNamedTypes <- filter (areEquivalentNamedTypes (typeId, (Fix namedType, scheme))) . Map.toList . namedTypes <$> get
     case currentNamedTypes of
      [] -> do addNamedType typeId (Fix namedType) scheme
               return $ Fix namedType
      ((_, (otherNT, _)):_) -> return otherNT

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

-- | Unrolls (expands) a TName recursive type by plugging in the holes from the given list of types.
-- Similar to instantiation, but uses a pre-defined set of type instances instead of using fresh
-- type variables.
unrollName :: Pos.SourcePos -> TypeId -> [Type] -> Infer Type
unrollName a tid ts =
  do (TScheme qvars t) <- (fmap snd . Map.lookup tid . namedTypes <$> get) `failWithM` throwError a "Unknown type id"
     let assocs = zip (map (Fix . TBody . TVar) qvars) ts
         tryLookup :: Eq a => [(a, a)] -> a -> a
         tryLookup pairs x = case lookup x pairs of
                              Nothing -> x
                              Just y -> y
         replace' :: Type -> Type
         replace' = tryLookup assocs
     return $ Fix $ fmap replace' (unFix t)

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
-- >>> runInferWith (InferState { nameSource = NameSource 2, varInstances = Map.empty, varSchemes = Map.empty, namedTypes = Map.empty }) . instantiate $ TScheme [0] (Fix $ TCons TFunc [Fix $ TBody (TVar 0), Fix $ TBody (TVar 1)])
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
instantiate :: TScheme -> Infer (Type)
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
generalize :: TypeEnv -> Type -> Infer TScheme
generalize tenv t = do
  unboundVars <- Set.difference (freeTypeVars t) <$> getFreeTVars tenv
  return $ TScheme (Set.toList unboundVars) t

----------------------------------------------------------------------
type UnifyF = Pos.SourcePos -> Type -> Type -> Infer TSubst

-- | Unifies given types, using the namedTypes from the infer state
--
-- >>> let p = Pos.initialPos "<dummy>"
-- >>> let u x y = runInfer $ unify p x y
-- >>> let fromRight (Right x) = x
--
-- >>> u (Fix $ TBody $ TVar 0) (Fix $ TBody $ TVar 1)
-- Right (fromList [(0,Fix (TBody (TVar 1)))])
-- >>> u (Fix $ TBody $ TVar 1) (Fix $ TBody $ TVar 0)
-- Right (fromList [(1,Fix (TBody (TVar 0)))])
--
-- >>> u (Fix $ TBody $ TNumber) (Fix $ TBody $ TVar 0)
-- Right (fromList [(0,Fix (TBody TNumber))])
-- >>> u (Fix $ TBody $ TVar 0) (Fix $ TBody $ TNumber)
-- Right (fromList [(0,Fix (TBody TNumber))])
--
-- >>> u (Fix $ TBody $ TVar 0) (Fix $ TRow $ TRowEnd $ Just $ RowTVar 1)
-- Right (fromList [(0,Fix (TRow (TRowEnd (Just (RowTVar 1)))))])
--
-- >>> u (Fix $ TBody $ TVar 0) (Fix $ TRow $ TRowProp "x" (Fix $ TBody TNumber) (TRowEnd $ Just $ RowTVar 1))
-- Right (fromList [(0,Fix (TRow (TRowProp "x" Fix (TBody TNumber) (TRowEnd (Just (RowTVar 1))))))])
--
-- >>> let row1 z = (Fix $ TRow $ TRowProp "x" (Fix $ TBody TNumber) (TRowEnd z))
-- >>> let sCloseRow = fromRight $ u (row1 $ Just $ RowTVar 1) (row1 Nothing)
-- >>> pretty $ applySubst sCloseRow (row1 $ Just $ RowTVar 1)
-- "{x: TNumber}"
--
-- Simple recursive type:
--
-- >>> let tvar0 = Fix $ TBody $ TVar 0
-- >>> let recRow = Fix $ TRow $ TRowProp "x" tvar0 (TRowEnd $ Just $ RowTVar 1)
-- >>> let s = fromRight $ u tvar0 recRow
-- >>> s
-- fromList [(0,Fix (TCons (TName (TypeId 1)) [Fix (TBody (TVar 1))]))]
-- >>> applySubst s tvar0
-- Fix (TCons (TName (TypeId 1)) [Fix (TBody (TVar 1))])
--
-- >>> :{
-- pretty $ runInfer $ do
--     s <- unify p tvar0 recRow
--     let (Fix (TCons (TName n1) targs1)) = applySubst s tvar0
--     t <- unrollName p n1 targs1
--     return t
-- :}
-- "{x: <Named Type: mu 'B'. b>, ..b}"
--
-- Unifying a rolled recursive type with its (unequal) unrolling should yield a null subst:
--
-- >>> :{
-- runInfer $ do
--     s <- unify p tvar0 recRow
--     let rolledT = applySubst s tvar0
--     let (Fix (TCons (TName n1) targs1)) = rolledT
--     unrolledT <- unrollName p n1 targs1
--     s2 <- unify p rolledT unrolledT
--     s3 <- unify p rolledT (applySubst s unrolledT)
--     s4 <- unify p (applySubst s rolledT) (applySubst s unrolledT)
--     s5 <- unify p (applySubst s rolledT) unrolledT
--     return (rolledT == unrolledT, s2, s3, s4, s5)
-- :}
-- Right (False,fromList [],fromList [],fromList [],fromList [])
--
-- >>> :{
-- pretty $ runInfer $ do
--     s1 <- unify p tvar0 recRow
--     tvar2 <- Fix . TBody . TVar <$> fresh
--     s2 <- unify p (applySubst s1 recRow) (applySubst s1 $ Fix $ TRow $ TRowProp "x" tvar2 (TRowEnd Nothing))
--     return $ applySubst (s2 `composeSubst` s1) recRow
-- :}
-- "{x: <Named Type: mu 'B'. {}>}"
--
-- >>> let rec2 = Fix $ TCons TFunc [recRow, Fix $ TBody TNumber]
-- >>> :{
-- pretty $ runInfer $ do
--     s1 <- unify p tvar0 rec2
--     return $ applySubst s1 rec2
-- :}
-- "(this: {x: <Named Type: mu 'B'. b>, ..b} -> TNumber)"
--
-- >>> :{
-- runInfer $ do
--     s1 <- unify p tvar0 rec2
--     s2 <- unify p tvar0 rec2
--     return $ (applySubst s1 rec2 == applySubst s2 rec2)
-- :}
-- Right True
--
unify :: UnifyF
unify = decycle3 unify''

unify'' :: Maybe UnifyF -> UnifyF
unify'' Nothing _ _ _ = return nullSubst
unify'' (Just recurse) a t1 t2 =
  do traceLog ("unifying: " ++ pretty t1 ++ " ~ " ++ pretty t2) ()
     tr' <$> unify' recurse a (unFix t1) (unFix t2)
  where tr' x = trace (tr2 x) x
        tr2 x = "unify: \t" ++ pretty t1 ++ " ,\t " ++ pretty t2 ++ "\n\t-->" ++ concatMap pretty (Map.toList x)

unificationError :: (VarNames x, Pretty x) => Pos.SourcePos -> x -> x -> Infer b
unificationError pos x y = throwError pos $ "Could not unify: " ++ pretty a ++ " with " ++ pretty b
  where [a, b] = minifyVars [x, y]

unify' :: UnifyF -> Pos.SourcePos -> FType (Fix FType) -> FType (Fix FType) -> Infer TSubst
unify' _ a (TBody (TVar n)) t = varBind a n (Fix t)
unify' _ a t (TBody (TVar n)) = varBind a n (Fix t)
unify' _ a (TBody x) (TBody y) = if x == y
                                       then return nullSubst
                                       else unificationError a x y
unify' recurse a (TCons (TName n1) targs1) (TCons (TName n2) targs2) =
  do if n1 == n2
     then return nullSubst
     else
       do let unroll' n' targs' = unrollName a n' targs'
          t1' <- unroll' n1 targs1
          t2' <- unroll' n2 targs2
          recurse a t1' t2' -- unificationError a (t1, t1') (t1, t2')
unify' recurse a (TCons (TName n1) targs1) t2 =
  do t1' <- unrollName a n1 targs1
     recurse a t1' (Fix t2) -- unificationError a (unFix t1') t2
unify' recurse a t1 t2@(TCons (TName _) []) = recurse a (Fix t2) (Fix t1)

unify' _ a t1@(TBody _) t2@(TCons _ _) = unificationError a t1 t2
unify' recurse a t1@(TCons _ _) t2@(TBody _) = recurse a (Fix t2) (Fix t1)
unify' recurse a t1@(TCons n1 ts1) t2@(TCons n2 ts2) =
    if (n1 == n2) && (length ts1 == length ts2)
    then fmap (tracePretty ("unified TCons's (" ++ show n1 ++ "): ")) <$> unifyl recurse a nullSubst $ zip ts1 ts2
    else unificationError a t1 t2 --throwError $ "TCons names or number of parameters do not match: " ++ pretty n1 ++ " /= " ++ pretty n2
unify' _ a t1@(TRow _)    t2@(TCons _ _) = unificationError a t1 t2
unify' _ a t1@(TRow _)    t2@(TBody _)   = unificationError a t1 t2
unify' _ a t1@(TCons _ _) t2@(TRow _)    = unificationError a t1 t2
unify' _ a t1@(TBody _)   t2@(TRow _)    = unificationError a t1 t2
-- TODO: un-hackify!
unify' recurse a t1@(TRow row1) t2@(TRow row2) =
  if t1 == t2
  then return nullSubst
  else
    do let (m2, r2) = flattenRow row2
           names2 = Set.fromList $ Map.keys m2
           (m1, r1) = flattenRow row1
           names1 = Set.fromList $ Map.keys m1
           commonNames = Set.toList $ names1 `Set.intersection` names2
--           namesToTypes :: Map EPropName (Type a) -> [EPropName] -> [Type a]
           namesToTypes m = mapMaybe $ flip Map.lookup m
--           commonTypes :: [(Type, Type)]
           commonTypes = zip (namesToTypes m1 commonNames) (namesToTypes m2 commonNames)
       s1 <- unifyl recurse a nullSubst commonTypes
       r <- RowTVar <$> fresh
       s2 <- unifyRows recurse a r s1 (t1, names1, m1) (t2, names2, r2)
       let s2' = s2 `composeSubst` s1
       s3 <- unifyRows recurse a r s2' (tracePretty "t2" $ t2, names2, m2) (tracePretty "t1" $ t1, names1, r1)
       return $ (tracePretty "unified rows subst result") $ s3 `composeSubst` s2'

unifyRows :: (VarNames x, Pretty x) => UnifyF -> Pos.SourcePos -> RowTVar -> TSubst
               -> (x, Set EPropName, Map EPropName (Type))
               -> (x, Set EPropName, FlatRowEnd Type)
               -> Infer TSubst
unifyRows recurse a r s1 (t1, names1, m1) (t2, names2, r2) =
    do let in1NotIn2 = names1 `Set.difference` names2
           rowTail = case r2 of
                      FlatRowEndTVar (Just _) -> FlatRowEndTVar $ Just r
                      _ -> r2
--                                              fmap (const r) r2
           in1NotIn2row = tracePretty "in1NotIn2row" $ applySubst s1 . Fix . TRow . unflattenRow m1 rowTail $ flip Set.member in1NotIn2

       case r2 of
         FlatRowEndTVar Nothing -> if Set.null in1NotIn2
                    then varBind a (getRowTVar r) (Fix $ TRow $ TRowEnd Nothing)
                    else unificationError a t1 t2
         FlatRowEndTVar (Just r2') -> recurse a (in1NotIn2row) (applySubst s1 $ Fix . TBody . TVar $ getRowTVar r2')
         FlatRowEndRec tid ts -> recurse a (in1NotIn2row) (applySubst s1 $ Fix $ TCons (TName tid) ts)

-- | Unifies pairs of types, accumulating the substs
unifyl :: UnifyF -> Pos.SourcePos -> TSubst -> [(Type, Type)] -> Infer TSubst
unifyl r a s ts = foldM (unifyl' r a) s ts

unifyl' :: UnifyF -> Pos.SourcePos -> TSubst -> (Type, Type) -> Infer TSubst
unifyl' recurse a s (x, y) = do
  traceLog ("step in unifyl got subst: " ++ pretty s) ()
  s' <- recurse a (tracePretty "--1" $ applySubst s x) (tracePretty "--2" $ applySubst s y)
  return $ tracePretty "step output in unifyl: " $ s' `composeSubst` s

newtype OrBool = OrBool { unOrBool :: Bool }
                 deriving (Eq, Show, Ord)
instance Monoid OrBool where
  mempty = OrBool False
  (OrBool x) `mappend` (OrBool y) = OrBool (x || y)

-- | Checks if a type var name appears as a free type variable nested somewhere inside a row type.
--
-- >>> isInsideRowType 0 (Fix (TBody $ TVar 0))
-- False
-- >>> isInsideRowType 0 (Fix (TRow $ TRowEnd (Just $ RowTVar 0)))
-- True
-- >>> isInsideRowType 0 (Fix (TRow $ TRowEnd (Just $ RowTVar 1)))
-- False
-- >>> isInsideRowType 0 (Fix (TCons TFunc [Fix $ TBody $ TVar 0, Fix $ TRow $ TRowEnd (Just $ RowTVar 1)]))
-- False
-- >>> isInsideRowType 0 (Fix (TCons TFunc [Fix $ TBody $ TVar 1, Fix $ TRow $ TRowEnd (Just $ RowTVar 0)]))
-- True
isInsideRowType :: TVarName -> Type -> Bool
isInsideRowType n (Fix t) =
  case t of
   TRow t' -> n `Set.member` freeTypeVars t'
   _ -> unOrBool $ fst (traverse (\x -> (OrBool $ isInsideRowType n x, x)) t)

varBind :: Pos.SourcePos -> TVarName -> Type -> Infer TSubst
varBind a n t | t == Fix (TBody (TVar n)) = return nullSubst
              | isInsideRowType n t =
                  do traceLog ("===> Generalizing mu-type: " ++ pretty n ++ " = " ++ pretty t) ()
                     namedType <- getNamedType n t
                     -- let (TCons (TName n1) targs1) = unFix namedType
                     -- t' <- unrollName a n1 targs1
                     return $ singletonSubst n namedType
              | n `Set.member` freeTypeVars t = throwError a $ "Occurs check failed: " ++ pretty n ++ " in " ++ pretty t
              | otherwise = return $ singletonSubst n t

-- | Drops the last element of a list. Does not entail an O(n) price.
-- >>> dropLast [1,2,3]
-- [1,2]
dropLast :: [a] -> [a]
dropLast [] = []
dropLast [_] = []
dropLast (x:xs) = x : dropLast xs

unifyAll :: Pos.SourcePos -> TSubst -> [Type] -> Infer TSubst
unifyAll a s ts = unifyl unify a s $ zip (dropLast ts) (drop 1 ts)


isExpansive :: Exp a -> Bool
isExpansive (EVar _ _)        = True
isExpansive (EApp _ _ _)      = True
isExpansive (EAssign _ _ _ _) = True
isExpansive (EPropAssign _ _ _ _ _) = True
isExpansive (EIndexAssign _ _ _ _ _) = True
isExpansive (ELet _ _ _ _)    = True
isExpansive (EAbs _ _ _)      = False
isExpansive (ELit _ _)        = False
isExpansive (EArray _ _)  = True
isExpansive (ETuple _ _)  = True
isExpansive (ERow _ _ _)    = True
isExpansive (EIfThenElse _ e1 e2 e3) = any isExpansive [e1, e2, e3]
isExpansive (EProp _ _ _)  = True
isExpansive (EIndex _ _ _)  = True
isExpansive (ENew _ _ _) = True
----------------------------------------------------------------------

closeRowList :: TRowList Type -> TRowList Type
closeRowList (TRowProp n t rest) = TRowProp n t (closeRowList rest)
closeRowList (TRowEnd _) = TRowEnd Nothing

-- | Replaces a top-level open row type with the closed equivalent.
-- >>> closeRow (Fix $ TRow $ TRowProp "a" (Fix $ TRow $ TRowProp "a.a" (Fix $ TBody TNumber) (TRowEnd (Just $ RowTVar 1))) (TRowEnd (Just $ RowTVar 2)))
-- Fix (TRow (TRowProp "a" Fix (TRow (TRowProp "a.a" Fix (TBody TNumber) (TRowEnd (Just (RowTVar 1))))) (TRowEnd Nothing)))
-- >>> closeRow (Fix $ TCons TFunc [Fix $ TRow $ TRowProp "a" (Fix $ TRow $ TRowProp "a.a" (Fix $ TBody TNumber) (TRowEnd Nothing)) (TRowEnd Nothing), Fix $ TBody TString])
-- Fix (TCons TFunc [Fix (TRow (TRowProp "a" Fix (TRow (TRowProp "a.a" Fix (TBody TNumber) (TRowEnd Nothing))) (TRowEnd Nothing))),Fix (TBody TString)])
-- >>> closeRow (Fix $ TCons TFunc [Fix $ TRow $ TRowProp "a" (Fix $ TRow $ TRowProp "a.a" (Fix $ TBody TNumber) (TRowEnd (Just $ RowTVar 1))) (TRowEnd (Just $ RowTVar 2)), Fix $ TBody TString])
-- Fix (TCons TFunc [Fix (TRow (TRowProp "a" Fix (TRow (TRowProp "a.a" Fix (TBody TNumber) (TRowEnd (Just (RowTVar 1))))) (TRowEnd (Just (RowTVar 2))))),Fix (TBody TString)])
closeRow :: Type -> Type
closeRow (Fix (TRow r)) = Fix . TRow $ closeRowList r
closeRow t = t

----------------------------------------------------------------------

-- For efficiency reasons, types list is returned in reverse order.
accumInfer :: TSubst -> TypeEnv -> [Exp Pos.SourcePos] -> Infer (TSubst, [(Type, Exp (Pos.SourcePos, Type))])
accumInfer initialSubst env =
  do traceLog ("accumInfer: initialSubst: " ++ pretty initialSubst ++ ", env: " ++ pretty env) ()
     foldM accumInfer' (initialSubst, [])
     where accumInfer' (subst, types) expr =
             do (subst', t, e) <- inferType env expr
                applySubstInfer subst'
                return (subst' `composeSubst` subst, (applySubst subst t,e):types)

inferType  :: TypeEnv -> Exp Pos.SourcePos -> Infer (TSubst, Type, Exp (Pos.SourcePos, Type))
inferType env expr = do
  traceLog (">> " ++ pretty expr) ()
  (s, t, e) <- inferType' env expr
  state <- get
  let tr = trace $ "<< " ++ pretty expr ++ " :: " ++ pretty t ++ "\n" ++ pretty state ++ "\n\t Environment: " ++ pretty env ++ "\n----------"
  return . tr $ (s, t, e)

inferType' :: TypeEnv -> Exp Pos.SourcePos -> Infer (TSubst, Type, Exp (Pos.SourcePos, Type))
inferType' _ (ELit a lit) = do
  let t = Fix $ TBody $ case lit of
                    LitNumber _ -> TNumber
                    LitBoolean _ -> TBoolean
                    LitString _ -> TString
                    LitRegex _ _ _ -> TRegex
                    LitUndefined -> TUndefined
                    LitNull -> TNull
  return (nullSubst, t, ELit (a,t) lit)
inferType' env (EVar a n) = do
  t <- instantiateVar a n env
  return (nullSubst, t, EVar (a, t) n)
inferType' env (EAbs a argNames e2) =
  do argTypes <- forM argNames (const $ Fix . TBody . TVar <$> fresh)
     env' <- foldM (\e (n, t) -> addVarScheme e n $ TScheme [] t) env $ zip argNames argTypes
     (s1, t1, e2') <- inferType env' e2
     applySubstInfer s1
     let t = Fix $ TCons TFunc $ map (applySubst s1) argTypes ++ [t1]
     return (s1, t, EAbs (a, t) argNames e2')
inferType' env (EApp a e1 eArgs) =
  do tvarName <- fresh
     let tvar = Fix $ TBody (TVar tvarName)
     (s1, t1, e1') <- inferType env e1
     applySubstInfer s1
     (s2, argsTE) <- tracePretty "EApp: unify type args" <$> accumInfer s1 env eArgs
     applySubstInfer s2
     let rargsTE = reverse argsTE
         tArgs = map fst rargsTE
         eArgs' = map snd rargsTE
         s2' = s2 `composeSubst` s1
     s3 <- tracePretty "EApp: unify inferred with template" <$> unify a (applySubst s2' t1) (applySubst s2' $ Fix . TCons TFunc $ tArgs ++ [tvar])
     let s3' = s3 `composeSubst` s2'
         t = applySubst s3' tvar
     applySubstInfer s3'
     return (tracePretty "\\ unified app, subst: " $ s3', t, EApp (a, t) e1' eArgs')
inferType' env (ENew a e1 eArgs) =
  do (s1, t1, e1') <- inferType env e1
     applySubstInfer s1
     (s2, argsTE) <- accumInfer s1 env eArgs
     applySubstInfer s2
     thisTVarName <- fresh
     resT <- Fix . TBody . TVar <$> fresh
     let thisT = Fix . TBody $ TVar thisTVarName
         rargsTE = reverse argsTE
         tArgs = thisT : map fst rargsTE
         eArgs' = map snd rargsTE
         s2' = s2 `composeSubst` s1
     s3 <- tracePretty "ENew: unify inferred with template" <$> unify a (applySubst s2' t1) (applySubst s2' $ Fix . TCons TFunc $ tArgs ++ [resT])
     let s3' = s3 `composeSubst` s2'
         t = applySubst s3' thisT
     applySubstInfer s3'
     s4 <- unify a t (closeRow t)
     applySubstInfer s4
     let s4' = s4 `composeSubst` s3'
         t' = applySubst s4' t
     return (s4', t', ENew (a, t') e1' eArgs')
inferType' env (ELet a n e1 e2) =
  do recType <- Fix . TBody . TVar <$> fresh
     recEnv <- addVarScheme env n $ TScheme [] recType
     (s1, t1, e1') <- inferType recEnv e1
     applySubstInfer s1
     s1rec <- unify a t1 (applySubst s1 recType)
     applySubstInfer s1rec
     let s1' = s1rec `composeSubst` s1
         generalizeScheme = tracePretty ("let generalized '" ++ pretty n ++ "' --") <$> generalize env (applySubst s1' t1)
     t' <- if isExpansive e1
           then return $ TScheme [] $ applySubst s1' t1
           else generalizeScheme
     env' <- addVarScheme env n t'
     (s2, t2, e2') <- inferType env' e2
     applySubstInfer s2
     let s2' = s2 `composeSubst` s1'
         t = applySubst s2' t2
     return (s2', t, ELet (a, t) n e1' e2')
-- | Handling of mutable variable assignment.
-- | Prevent mutable variables from being polymorphic.
inferType' env (EAssign a n expr1 expr2) =
  do varId <- getVarId n env `failWith` throwError a ("Assertion failed, missing varId for var: '" ++ show n ++ "'")
     lvalueScheme <- getVarScheme a n env `failWithM` throwError a ("Unbound variable: " ++ show n ++ " in assignment " ++ pretty expr1)
     let ungeneralizedScheme = ungeneralize lvalueScheme
     lvalueT <- instantiate ungeneralizedScheme
     setVarScheme n varId ungeneralizedScheme
     (s1, rvalueT, expr1') <- inferType env expr1
     s2 <- unify a rvalueT (applySubst s1 lvalueT)
     let s3 = s2 `composeSubst` s1
     s4 <- unifyAllInstances a s3 $ getQuantificands lvalueScheme
     applySubstInfer s4
     (s5, tRest, expr2') <- inferType env expr2
     return (s5 `composeSubst` s4, tRest, EAssign (a, tRest) n expr1' expr2')
inferType' env (EPropAssign a objExpr n expr1 expr2) =
  do (s1, objT, objExpr') <- inferType env objExpr
     applySubstInfer s1
     (s2, rvalueT, expr1') <- inferType env expr1
     applySubstInfer s2
     let s2' = s2 `composeSubst` s1
     rowTailVar <- RowTVar <$> fresh
     s3 <- unify a (applySubst s2' objT) $ applySubst s2' . Fix . TRow $ TRowProp n rvalueT $ TRowEnd (Just rowTailVar)
     applySubstInfer s3
     let s3' = s3 `composeSubst` s2'
     (s4, expr2T, expr2') <- inferType env expr2
     let s5 = s4 `composeSubst` s3'
     s6 <- unifyAllInstances a s5 [getRowTVar rowTailVar]
     return (s6, applySubst s6 expr2T, EPropAssign (a, applySubst s6 expr2T) objExpr' n expr1' expr2')
inferType' env (EIndexAssign a eArr eIdx expr1 expr2) =
  do (s1, tArr, eArr') <- inferType env eArr
     elemTVarName <- fresh
     let elemType = Fix . TBody . TVar $ elemTVarName
     s1' <- unify a (Fix $ TCons TArray [elemType]) tArr
     let s1'' = s1' `composeSubst` s1
     applySubstInfer s1''
     (s2, tId, eIdx') <- inferType env eIdx
     s2' <- unify a (Fix $ TBody TNumber) tId
     let s2'' = s2' `composeSubst` s2 `composeSubst` s1''
     applySubstInfer s2''
     let elemType' = applySubst s2'' elemType
     (s3, tExpr1, expr1') <- inferType env expr1
     s3' <- unify a tExpr1 elemType'
     let s3'' = s3' `composeSubst` s3 `composeSubst` s2''
     applySubstInfer s3''
     s3''' <- unifyAllInstances a s3'' [elemTVarName]
     let s3b = s3''' `composeSubst` s3''
     applySubstInfer s3b
     (s4, tExpr2, expr2') <- inferType env expr2
     let s4' = s4 `composeSubst` s3b
     applySubstInfer s4'
     return (s4', applySubst s4' tExpr2 , EIndexAssign (a, applySubst s4' elemType')  eArr' eIdx' expr1' expr2')
inferType' env (EArray a exprs) =
  do tvName <- fresh
     let tv = Fix . TBody $ TVar tvName
     (subst, te) <- accumInfer nullSubst env exprs
     let types = map fst te
     subst' <- unifyl unify a subst $ zip (tv:types) types
     applySubstInfer subst'
     let t = Fix $ TCons TArray [applySubst subst' $ Fix . TBody $ TVar tvName]
     return (subst', t, EArray (a,t) $ map snd te)
inferType' env (ETuple a exprs) =
  do (subst, te) <- accumInfer nullSubst env exprs
     let t = Fix . TCons TTuple . reverse $ map fst te
     return (subst, t, ETuple (a,t) $ map snd te)
inferType' env (ERow a isOpen propExprs) =
  do (s, te) <- accumInfer nullSubst env $ map snd propExprs
     applySubstInfer s
     endVar <- RowTVar <$> fresh
     let propNamesTypes = zip (map fst propExprs) (reverse $ map fst te)
         rowEnd' = TRowEnd $ if isOpen then Just endVar else Nothing
         rowType = Fix . TRow $ foldr (\(n,t') r -> TRowProp n t' r) rowEnd' propNamesTypes
         t = applySubst s rowType
     return (s, t, ERow (a,t) isOpen $ zip (map fst propExprs) (map snd te))
inferType' env (EIfThenElse a ePred eThen eElse) =
  do (s1, tp, ePred') <- inferType env ePred
     s2 <- unify a (Fix $ TBody TBoolean) tp
     let s3 = s2 `composeSubst` s1
     applySubstInfer s3
     (s4, tThen, eThen') <- inferType env eThen
     applySubstInfer s4
     (s5, tElse, eElse') <- inferType env eElse
     s6 <- unify a tThen tElse
     let s' = s6 `composeSubst` s5 `composeSubst` s4 `composeSubst` s3
     applySubstInfer s'
     return (s', tThen, EIfThenElse (a, tThen) ePred' eThen' eElse')
inferType' env (EProp a eObj propName) =
  do (s1, tObj, eObj') <- inferType env eObj
     rowVar <- RowTVar <$> fresh
     propVar <- fresh
     s2 <- unify a tObj $ Fix . TRow $ TRowProp propName (Fix . TBody $ TVar propVar) $ TRowEnd (Just rowVar)
     let s3 = s2 `composeSubst` s1
         t = applySubst s3 (Fix . TBody $ TVar propVar)
     applySubstInfer s3
     return (s3, t, EProp (a,t) eObj' propName)
inferType' env (EIndex a eArr eIdx) =
  do (s1, tArr, eArr') <- inferType env eArr
     elemType <- Fix . TBody . TVar <$> fresh
     s1' <- unify a (Fix $ TCons TArray [elemType]) tArr
     let s1'' = s1' `composeSubst` s1
     applySubstInfer s1''
     (s2, tId, eIdx') <- inferType env eIdx
     s2' <- unify a (Fix $ TBody TNumber) tId
     let s2'' = s2' `composeSubst` s2
     applySubstInfer s2''
     let elemType' = applySubst s2'' elemType
     return (s2'' `composeSubst` s1'', elemType' , EIndex (a, elemType')  eArr' eIdx')

unifyAllInstances :: Pos.SourcePos -> TSubst -> [TVarName] -> Infer TSubst
unifyAllInstances a s tvs = do
  m <- varInstances <$> get
  let equivalenceSets = mapMaybe (`Map.lookup` m) tvs

  -- TODO suboptimal - some of the sets may be identical
  let unifyAll' s' equivs = unifyAll a s' . tracePretty "equivalence:" $ Set.toList equivs
  tracePretty "unified equivs:" <$> foldM unifyAll' s equivalenceSets

minifyVarsFunc :: (VarNames a) => a -> TVarName -> TVarName
minifyVarsFunc xs n = fromMaybe n $ Map.lookup n vars
  where vars = Map.fromList $ zip (Set.toList $ freeTypeVars xs) ([1..] :: [TVarName])

minifyVars :: (VarNames a) => a -> a
minifyVars xs = mapVarNames (minifyVarsFunc xs) xs

createEnv :: Map EVarName TScheme -> Infer (Map EVarName VarId)
createEnv builtins = foldM addVarScheme' Map.empty $ Map.toList builtins
    where allTVars :: TScheme -> Set TVarName
          allTVars (TScheme qvars t) = freeTypeVars t `Set.union` (Set.fromList qvars)
          safeLookup :: Eq a => [(a,a)] -> a -> a
          safeLookup assoc n = fromMaybe n $ lookup n assoc
          addVarScheme' :: Map EVarName VarId -> (EVarName, TScheme) -> Infer (Map EVarName VarId)
          addVarScheme' m (name, tscheme) = do
            allocNames <- forM (Set.toList $ allTVars tscheme) $ \tvName -> (fresh >>= return . (tvName,))
            addVarScheme m name $ mapVarNames (safeLookup allocNames) tscheme


typeInference :: Map EVarName TScheme -> Exp Pos.SourcePos -> Infer (Exp (Pos.SourcePos, Type))
typeInference builtins e = do
  env <- createEnv builtins
  (_s, _t, e') <- inferType env e
  let e'' = (fmap . fmap) (applySubst _s) e'
  return e''

----------------------------------------------------------------------
--
-- | Mutable variable being assigned incompatible types:
--
-- >>> let p = Pos.initialPos "<dummy>"
-- >>> let fun = EAbs p
-- >>> let var = EVar p
-- >>> let let' = ELet p
-- >>> let tuple = ETuple p
-- >>> let app a b = EApp p a [b]
-- >>> let lit = ELit p
-- >>> let assign = EAssign p
-- >>> let array = EArray p
--
-- x is known to have type forall a. a -> a, and to have been used in a context requiring bool -> bool (e.g. `x True`)
--
-- we now try to assign x := \y -> 2
--
-- This should fail because it "collapses" x to be Number -> Number which is not compatible with bool -> bool
--
-- >>> test $ let' "x" (fun ["z"] (var "z")) (let' "y" (tuple [app (var "x") (lit (LitNumber 2)), app (var "x") (lit (LitBoolean True))]) (assign "x" (fun ["y"] (lit (LitNumber 0))) (tuple [var "x", var "y"])))
-- "<dummy>:1:1: Error: Could not unify: TNumber with TBoolean"
--
-- The following should succeed because x is immutable and thus polymorphic:
--
-- >>> test $ let' "x" (fun ["z"] (var "z")) (let' "y" (tuple [app (var "x") (lit (LitNumber 2)), app (var "x") (lit (LitBoolean True))]) (tuple [var "x", var "y"]))
-- "((this: b -> b), (TNumber, TBoolean))"
--
-- The following should fail because x is mutable and therefore a monotype:
--
-- >>> test $ let' "x" (fun ["z"] (var "z")) (let' "y" (tuple [app (var "x") (lit (LitNumber 2)), app (var "x") (lit (LitBoolean True))]) (assign "x" (fun ["z1"] (var "z1")) (tuple [var "x", var "y"])))
-- "<dummy>:1:1: Error: Could not unify: TNumber with TBoolean"
--
-- The following should also succeed because "x" is only ever used like this: (x True). The second assignment to x is: x := \z1 -> False, which is specific but matches the usage. Note that x's type is collapsed to: Boolean -> Boolean.
--
-- >>> test $ let' "x" (fun ["z"] (var "z")) (let' "y" (app (var "x") (lit (LitBoolean True))) (assign "x" (fun ["z1"] (lit (LitBoolean False))) (tuple [var "x", var "y"])))
-- "((this: TBoolean -> TBoolean), TBoolean)"
--
-- | Tests a setter for x being called with something more specific than x's original definition:
-- >>> :{
-- >>> test $ let'
-- >>> "x" (fun ["a"] (var "a"))
-- >>> (let' "setX"
-- >>>    (fun ["v"]
-- >>>             (let'
-- >>>          "_" (assign "x" (var "v") (var "x")) (lit (LitBoolean False))))
-- >>>    (let'
-- >>>       "_" (app (var "setX") (fun ["a"] (lit (LitString "a"))))
-- >>>       (app (var "x") (lit (LitBoolean True)))))
-- >>> :}
-- "<dummy>:1:1: Error: Could not unify: TString with TBoolean"
--
-- >>> test $ tuple [lit (LitBoolean True), lit (LitNumber 2)]
-- "(TBoolean, TNumber)"
--
-- >>> test $ let' "id" (fun ["x"] (var "x")) (assign "id" (fun ["y"] (var "y")) (var "id"))
-- "(this: a -> a)"
--
-- >>> test $ let' "id" (fun ["x"] (var "x")) (assign "id" (lit (LitBoolean True)) (var "id"))
-- "<dummy>:1:1: Error: Could not unify: TBoolean with (this: a -> a)"
--
-- >>> test $ let' "x" (lit (LitBoolean True)) (assign "x" (lit (LitBoolean False)) (var "x"))
-- "TBoolean"
--
-- >>> test $ let' "x" (lit (LitBoolean True)) (assign "x" (lit (LitNumber 3)) (var "x"))
-- "<dummy>:1:1: Error: Could not unify: TNumber with TBoolean"
--
-- >>> test $ let' "x" (array [lit (LitBoolean True)]) (var "x")
-- "[TBoolean]"
--
-- >>> test $ let' "x" (array [lit $ LitBoolean True, lit $ LitBoolean False]) (var "x")
-- "[TBoolean]"
--
-- >>> test $ let' "x" (array []) (assign "x" (array []) (var "x"))
-- "[a]"
--
-- >>> test $ let' "x" (array [lit $ LitBoolean True, lit $ LitNumber 2]) (var "x")
-- "<dummy>:1:1: Error: Could not unify: TNumber with TBoolean"
--
-- >>> test $ let' "id" (fun ["x"] (let' "y" (var "x") (var "y"))) (app (var "id") (var "id"))
-- "(this: b -> b)"
--
-- >>> test $ let' "id" (fun ["x"] (let' "y" (var "x") (var "y"))) (app (app (var "id") (var "id")) (lit (LitNumber 2)))
-- "TNumber"
--
-- >>> test $ let' "id" (fun ["x"] (app (var "x") (var "x"))) (var "id")
-- "<dummy>:1:1: Error: Occurs check failed: a in (this: a -> b)"
--
-- >>> test $ fun ["m"] (let' "y" (var "m") (let' "x" (app (var "y") (lit (LitBoolean True))) (var "x")))
-- "((this: TBoolean -> a) -> a)"
--
-- >>> test $ app (lit (LitNumber 2)) (lit (LitNumber 2))
-- "<dummy>:1:1: Error: Could not unify: TNumber with (TNumber -> a)"
--
-- EAssign tests
-- >>> test $ let' "x" (fun ["y"] (lit (LitNumber 0))) (assign "x" (fun ["y"] (var "y")) (var "x"))
-- "(this: TNumber -> TNumber)"
--
-- >>> test $ let' "x" (fun ["y"] (var "y")) (assign "x" (fun ["y"] (lit (LitNumber 0))) (var "x"))
-- "(this: TNumber -> TNumber)"
--
-- >>> test $ let' "x" (fun ["y"] (var "y")) (tuple [app (var "x") (lit (LitNumber 2)), app (var "x") (lit (LitBoolean True))])
-- "(TNumber, TBoolean)"
--
-- >>> test $ let' "x" (fun ["y"] (var "y")) (app (var "x") (var "x"))
-- "(this: b -> b)"
--
-- >>> test $ let' "x" (fun ["a"] (var "a")) (let' "getX" (fun ["v"] (var "x")) (let' "setX" (fun ["v"] (let' "_" (assign "x" (var "v") (var "x")) (lit (LitBoolean True)))) (let' "_" (app (var "setX") (fun ["a"] (lit (LitString "a")))) (var "getX"))))
-- "(this: b -> (TString -> TString))"
test :: Exp Pos.SourcePos -> String
test e = case runTypeInference e of
          Left err -> pretty err
          Right expr -> pretty $ snd . head . getAnnotations . minifyVars $ expr


runTypeInference :: Exp Pos.SourcePos -> Either TypeError (Exp (Pos.SourcePos, Type))
runTypeInference e = runInfer $ typeInference Builtins.builtins e


#ifdef QUICKCHECK

-- Test runner

return []


instance (Ord k, Arbitrary k, Arbitrary v) => Arbitrary (Map k v) where
    arbitrary = Map.fromList <$> resize 2 arbitrary
    shrink m = map (flip Map.delete m) (Map.keys m)

$( derive makeArbitrary ''TRowList )
$( derive makeArbitrary ''TConsName )
$( derive makeArbitrary ''TBody )
$( derive makeArbitrary ''Type )

runAllTests :: IO Bool
runAllTests = $(quickCheckAll)

#endif
