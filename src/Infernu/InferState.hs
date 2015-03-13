{-# LANGUAGE CPP             #-}
{-# LANGUAGE TupleSections   #-}
{-# LANGUAGE BangPatterns    #-}

module Infernu.InferState
       where

import           Control.Monad              (foldM, forM, forM_, liftM2, when)
import           Control.Monad.Trans        (lift)
import           Control.Monad.Trans.Either (EitherT (..), left, runEitherT)
import           Control.Monad.Trans.State  (StateT (..), evalStateT, get, put, modify)
import           Data.Foldable              (Foldable (..), msum)
import           Data.Traversable              (Traversable (..))
import           Data.Monoid (mappend)
    
import           Data.Functor               ((<$>))
import           Data.Functor.Identity      (Identity (..), runIdentity)
import qualified Data.Map.Lazy              as Map
-- import           Data.Map.Lazy              (Map)
import           Data.Maybe                 (fromMaybe)
import qualified Data.Set                   as Set
import           Data.Set                   (Set)
import           Prelude                    hiding (foldr, sequence, mapM)



import           Infernu.Pretty
import           Infernu.Types
import           Infernu.Log

    
-- | Inference monad. Used as a stateful context for generating fresh type variable names.
type Infer a = StateT InferState (EitherT TypeError Identity) a

runInferWith :: InferState -> Infer a -> Either TypeError a
runInferWith ns inf = runIdentity . runEitherT $ evalStateT inf ns
    -- where inf' = do res <- inf
    --                 unis <- getPendingUnifications
    --                 forM unis $ \((a, scheme), t) ->
    --                                 do inst <- instantiate scheme --  >>= assertNoPred
    --                                    unify a inst t

runSubInfer :: Infer a -> Infer (Either TypeError a)
runSubInfer a = do
  s <- get
  return $ runInferWith s a

getState :: Infer InferState
getState = get

setState :: InferState -> Infer ()
setState = put
           
runInfer :: Infer a -> Either TypeError a
runInfer = runInferWith emptyInferState

fresh :: Infer TVarName
fresh = do
  modify $ \is -> is { nameSource = (nameSource is) { lastName = lastName (nameSource is) + 1 } }
  lastName . nameSource <$> get

freshVarId :: Infer VarId
freshVarId = VarId <$> fresh

throwError :: Source -> String -> Infer a
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

getVarScheme :: Source -> EVarName -> TypeEnv -> Infer (Maybe TypeScheme)
getVarScheme a n env = case getVarId n env of
                       Nothing -> throwError a $ "Unbound variable: '" ++ show n ++ "'"
                       Just varId -> getVarSchemeByVarId varId

setVarScheme :: TypeEnv -> EVarName -> TypeScheme -> VarId -> Infer TypeEnv
setVarScheme env n scheme varId = do
  modify $ \is -> is { varSchemes = trace ("Inserting scheme for " ++ pretty n ++ ": " ++ pretty scheme) . Map.insert varId scheme $ varSchemes is }
  return $ Map.insert n varId env

addVarScheme :: TypeEnv -> EVarName -> TypeScheme -> Infer TypeEnv
addVarScheme env n scheme = do
  varId <- tracePretty ("-- '" ++ pretty n ++ "' = varId") <$> freshVarId
  setVarScheme env n scheme varId

--addPendingUnification :: (Source, Type, (ClasSet TypeScheme) -> Infer ()
addPendingUnification :: (Source, Type, (ClassName, Set TypeScheme)) -> Infer ()
addPendingUnification ts = do
    modify $ \is -> is { pendingUni = Set.insert ts $ pendingUni is }
    return ()

getPendingUnifications :: Infer (Set (Source, Type, (ClassName, Set TypeScheme)))
getPendingUnifications = pendingUni <$> get

setPendingUnifications :: (Set (Source, Type, (ClassName, Set TypeScheme))) -> Infer ()
setPendingUnifications ts = do
    modify $ \is -> is { pendingUni = ts }
    return ()
        
----------------------------------------------------------------------


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
  traceLog ("===> Introducing named type: " ++ pretty tid ++ " => " ++ pretty scheme)
  modify $ \is -> is { namedTypes = Map.insert tid (t, scheme) $ namedTypes is }
  return ()

-- | Compares schemes up to alpha equivalence including named type constructors equivalence (TCons
-- TName...).
-- 
-- >>> let mkNamedType tid ts = Fix $ TCons (TName (TypeId tid)) ts
--  
-- >>> areEquivalentNamedTypes (mkNamedType 0 [], schemeEmpty (Fix $ TBody TNumber)) (mkNamedType 1 [], schemeEmpty (Fix $ TBody TString))
-- False
-- >>> areEquivalentNamedTypes (mkNamedType 0 [], schemeEmpty (mkNamedType 0 [])) (mkNamedType 1 [], schemeEmpty (mkNamedType 1 []))
-- True
-- >>> :{
--     areEquivalentNamedTypes (mkNamedType 0 [], schemeEmpty (Fix $ TFunc [Fix $ TBody TNumber] (mkNamedType 0 [])))
--                             (mkNamedType 1 [], schemeEmpty (Fix $ TFunc [Fix $ TBody TNumber] (mkNamedType 1 [])))
-- :}
-- True
--  
-- >>> :{
--     areEquivalentNamedTypes (mkNamedType 0 [Fix $ TBody $ TVar 10], TScheme [10] (qualEmpty $ Fix $ TFunc [Fix $ TBody $ TVar 10] (mkNamedType 0 [Fix $ TBody $ TVar 10])))
--                             (mkNamedType 1 [Fix $ TBody $ TVar 11], TScheme [11] (qualEmpty $ Fix $ TFunc [Fix $ TBody $ TVar 11] (mkNamedType 1 [Fix $ TBody $ TVar 11])))
-- :}
-- True
replaceFixQual
  :: (Functor f, Eq (f (Fix f))) =>
     f (Fix f) -> f (Fix f) -> TQual (Fix f) -> TQual (Fix f)
replaceFixQual src dest (TQual preds t) = TQual (map (\p -> p { predType = replaceFix src dest $ predType p}) preds) (replaceFix src dest t)

areEquivalentNamedTypes :: (Type, TypeScheme) -> (Type, TypeScheme) -> Bool
areEquivalentNamedTypes (t1, s1) (t2, s2) = s2 == (s2 { schemeType = applySubst subst $ replaceFixQual (unFix t1) (unFix t2) $ schemeType s1 })
  where subst = foldr (\(x,y) s -> singletonSubst x (Fix $ TBody $ TVar y) `composeSubst` s) nullSubst $ zip (schemeVars s1) (schemeVars s2)

-- Checks if a given type variable appears in the given type *only* as a parameter to a recursive
-- type name.  If yes, returns the name of recursive types (and position within) in which it
-- appears; otherwise returns Nothing.
--isRecParamOnly :: TVarName -> Maybe (TypeId, Int) -> Type -> Maybe [(TypeId, Int)]
isRecParamOnly
  :: (Num t, Enum t) =>
     TVarName -> Maybe (TypeId, t) -> Type -> Maybe [(TypeId, t)]
isRecParamOnly n1 typeId t1 =
  case unFix t1 of
   TBody (TVar n1') -> if n1' == n1 then sequence [typeId] else Just []
   TBody _ -> Just []
   TCons (TName typeId') subTs -> recurseIntoNamedType typeId' subTs
   TCons _ subTs -> msum $ map (isRecParamOnly n1 Nothing) subTs
   TFunc ts tres -> (isRecParamOnly n1 Nothing) tres `mappend` msum (map (isRecParamOnly n1 Nothing) ts)
   TRow rlist -> isRecParamRecList n1 rlist
     where isRecParamRecList n' rlist' =
             case rlist' of
              TRowEnd _ -> Just []
              -- TODO: assumes the quanitified vars in TScheme do not shadow other type variable names
              -- TODO: Can we safely ignore preds here?
              TRowProp _ (TScheme _ t') rlist'' -> liftM2 (++) (isRecParamOnly n1 Nothing $ qualType t') (isRecParamRecList n' rlist'')
              TRowRec typeId' subTs -> recurseIntoNamedType typeId' subTs
  where recurseIntoNamedType typeId' subTs = msum $ map (\(t,i) -> isRecParamOnly n1 (Just (typeId', i)) t) $ zip subTs [0..]

dropAt :: Integral a => a -> [b] -> [b]
dropAt _ [] = []
dropAt 0 (_:xs) = xs
dropAt n (_:xs) = dropAt (n-1) xs

replaceRecType :: TypeId -> TypeId -> Int -> Type -> Type
replaceRecType typeId newTypeId indexToDrop t1 =
    let replace' = replaceRecType typeId newTypeId indexToDrop
        mapTs' = map replace'
    in  case unFix t1 of
            TBody _ -> t1
            TCons (TName typeId') subTs -> if typeId == typeId'
                                          then Fix $ TCons (TName newTypeId) $ dropAt indexToDrop subTs
                                          else t1
            TCons n subTs -> Fix $ TCons n $ mapTs' subTs
            TFunc ts tres -> Fix $ TFunc (mapTs' ts) (replace' tres)
            TRow rlist -> Fix $ TRow $ go rlist
             where go rlist' =
                     case rlist' of
                      TRowEnd _ -> rlist'
                      TRowProp p (TScheme qv t') rlist'' -> TRowProp p (TScheme qv (t' { qualType = replace' $ qualType t' })) (go rlist'')
                      TRowRec tid ts -> if typeId == tid
                                        then TRowRec newTypeId $ dropAt indexToDrop ts
                                        else rlist'

allocNamedType :: TVarName -> Type -> Infer Type
allocNamedType n t =
  do typeId <- TypeId <$> fresh
     let namedType = TCons (TName typeId) $ map (Fix . TBody . TVar) $ Set.toList $ freeTypeVars t `Set.difference` Set.singleton n
         target = replaceFix (TBody (TVar n)) namedType t
     scheme <- unsafeGeneralize Map.empty $ qualEmpty target
     traceLog $ "===> Generated scheme for mu type: " ++ pretty scheme
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
         sType' = (schemeType scheme) { qualType = replaceOldNamedType $ qualType $  schemeType scheme }
         newTs = dropAt ix $ ts
         newNamedType = Fix (TCons (TName newTypeId) newTs)
         --updatedNamedType = Fix (TCons (TName tid) newTs)
         updatedScheme = applySubst (singletonSubst n newNamedType) $ TScheme qVars'  sType'
         
     addNamedType newTypeId newNamedType updatedScheme
     -- TODO: we could alternatively update the existing named type, but that will break it's schema (will now take less params?)
     --addNamedType tid updatedNamedType updatedScheme
     return $ replaceOldNamedType t
     
     
getNamedType :: TVarName -> Type -> Infer Type
getNamedType n t =
  do let recTypeParamPos = isRecParamOnly n Nothing t
     traceLog ("isRecParamOnly: " ++ pretty n ++ " in " ++ pretty t ++ ": " ++ (show $ fmap pretty $ recTypeParamPos))
     case recTypeParamPos of
      Just [(tid, ix)] -> resolveSimpleMutualRecursion n t tid ix
      -- either the variable appears outside a recursive type's type parameter list, or it appears
      -- in more than one such position:
      _ -> allocNamedType n t 


unrollNameByScheme :: Substable a => [Type] -> [TVarName] -> a -> a
unrollNameByScheme ts qvars t = applySubst subst t
  where assocs = zip qvars ts
        subst = foldr (\(tvar,destType) s -> singletonSubst tvar destType `composeSubst` s) nullSubst assocs

-- | Unrolls (expands) a TName recursive type by plugging in the holes from the given list of types.
-- Similar to instantiation, but uses a pre-defined set of type instances instead of using fresh
-- type variables.
unrollName :: Source -> TypeId -> [Type] -> Infer QualType
unrollName a tid ts =
    -- TODO: Is it safe to ignore the scheme preds here?
    do (TScheme qvars t) <- (fmap snd . Map.lookup tid . namedTypes <$> get) `failWithM` throwError a "Unknown type id"
       return $ unrollNameByScheme ts qvars t
    
-- | Applies a subsitution onto the state (basically on the variable -> scheme map).
--
-- >>> :{
-- runInfer $ do
--     let t = TScheme [0] (TQual [] (Fix $ TFunc [Fix $ TBody (TVar 0)] (Fix $ TBody (TVar 1))))
--     let tenv = Map.empty
--     tenv' <- addVarScheme tenv "x" t
--     applySubstInfer $ Map.singleton 0 (Fix $ TBody TString)
--     varSchemes <$> get
-- :}
-- Right (fromList [(VarId 3,TScheme {schemeVars = [], schemeType = TQual {qualPred = [], qualType = Fix (TFunc [Fix (TBody TString)] Fix (TBody (TVar 1)))}})])
--
applySubstInfer :: TSubst -> Infer ()
applySubstInfer s =
  do traceLog ("applying subst: " ++ pretty s)
     modify $ applySubst s

-- | Instantiate a type scheme by giving fresh names to all quantified type variables.
--
-- For example:
--
-- >>> runInferWith (emptyInferState { nameSource = NameSource 2 }) . instantiate $ TScheme [0] (TQual { qualPred = [], qualType = Fix $ TFunc [Fix $ TBody (TVar 0)] (Fix $ TBody (TVar 1)) }) 
-- Right (TQual {qualPred = [], qualType = Fix (TFunc [Fix (TBody (TVar 3))] Fix (TBody (TVar 1)))})
--
-- In the above example, type variable 0 has been replaced with a fresh one (3), while the unqualified free type variable 1 has been left as-is.
--
-- >>> :{
-- runInfer $ do
--     let t = TScheme [0] (TQual [] (Fix $ TFunc [Fix $ TBody (TVar 0)] (Fix $ TBody (TVar 1))))
--     let tenv = Map.empty
--     tenv' <- addVarScheme tenv "x" t
--     instantiateVar emptySource "x" tenv'
-- :}
-- Right (TQual {qualPred = [], qualType = Fix (TFunc [Fix (TBody (TVar 4))] Fix (TBody (TVar 1)))})
--
instantiateScheme :: Bool -> TypeScheme -> Infer QualType
instantiateScheme shouldAddVarInstances (TScheme tvarNames t) = do
  allocNames <- forM tvarNames $ \tvName -> do
    freshName <- fresh
    return (tvName, freshName)
  when shouldAddVarInstances $ forM_ allocNames $ uncurry addVarInstance
  let replaceVar n = fromMaybe n $ lookup n allocNames
  return $ mapVarNames replaceVar t

instantiate :: TypeScheme -> Infer QualType
instantiate = instantiateScheme True

instantiateVar :: Source -> EVarName -> TypeEnv -> Infer QualType
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
-- >>> runInfer $ generalize (ELit "bla" LitUndefined) Map.empty $ qualEmpty $ Fix $ TFunc [Fix $ TBody (TVar 0)] (Fix $ TBody (TVar 1))
-- Right (TScheme {schemeVars = [0,1], schemeType = TQual {qualPred = [], qualType = Fix (TFunc [Fix (TBody (TVar 0))] Fix (TBody (TVar 1)))}})
--
-- >>> :{
-- runInfer $ do
--     let t = TScheme [1] (TQual [] (Fix $ TFunc [Fix $ TBody (TVar 0)] (Fix $ TBody (TVar 1))))
--     tenv <- addVarScheme Map.empty "x" t
--     generalize (ELit "bla" LitUndefined) tenv (qualEmpty $ Fix $ TFunc [Fix $ TBody (TVar 0)] (Fix $ TBody (TVar 2)))
-- :}
-- Right (TScheme {schemeVars = [2], schemeType = TQual {qualPred = [], qualType = Fix (TFunc [Fix (TBody (TVar 0))] Fix (TBody (TVar 2)))}})
--
-- In this example the steps were:
--
-- 1. Environment: { x :: forall 0. 0 -> 1 }
--
-- 2. generalize (1 -> 2)
--
-- 3. result: forall 2. 1 -> 2
--
-- >>> let expr = ELit "foo" LitUndefined
--
-- >>> runInfer $ generalize expr Map.empty (qualEmpty $ Fix $ TFunc [Fix $ TBody (TVar 0)] (Fix $ TBody (TVar 0)))
-- Right (TScheme {schemeVars = [0], schemeType = TQual {qualPred = [], qualType = Fix (TFunc [Fix (TBody (TVar 0))] Fix (TBody (TVar 0)))}})
--
-- >>> runInfer $ generalize expr Map.empty (TQual [TPredIsIn (ClassName "Bla") (Fix $ TBody (TVar 1))] (Fix $ TBody (TVar 0)))
-- Right (TScheme {schemeVars = [0,1], schemeType = TQual {qualPred = [TPredIsIn {predClass = ClassName "Bla", predType = Fix (TBody (TVar 1))}], qualType = Fix (TBody (TVar 0))}})
--
-- TODO add tests for monotypes
unsafeGeneralize :: TypeEnv -> QualType -> Infer TypeScheme
unsafeGeneralize tenv t = do
    traceLog $ "Generalizing: " ++ pretty t
    s <- getMainSubst
    let t' = applySubst s t
    unboundVars <- Set.difference (freeTypeVars t') <$> getFreeTVars tenv
    traceLog $ "Generalization result: unbound vars = " ++ pretty unboundVars ++ ", type = " ++ pretty t'
    return $ TScheme (Set.toList unboundVars) t'

isExpansive :: Exp a -> Bool
isExpansive (EVar _ _)        = False
isExpansive (EApp _ _ _)      = True
isExpansive (EAssign _ _ _ _) = True
isExpansive (EPropAssign _ _ _ _ _) = True
isExpansive (EIndexAssign _ _ _ _ _) = True
isExpansive (ELet _ _ _ _)    = True
isExpansive (EAbs _ _ _)      = False
isExpansive (ELit _ _)        = False
isExpansive (EArray _ _)  = True
isExpansive (ETuple _ _)  = True
isExpansive (EStringMap _ _)    = True
isExpansive (ERow _ _ _)    = True
isExpansive (EIfThenElse _ e1 e2 e3) = any isExpansive [e1, e2, e3]
isExpansive (EProp _ e _)  = isExpansive e
isExpansive (EIndex _ a b)  = any isExpansive [a, b]
isExpansive (ENew _ _ _) = True


generalize :: Exp a -> TypeEnv -> QualType -> Infer TypeScheme
generalize exp' env t = if isExpansive exp'
                        then return $ TScheme [] t
                        else unsafeGeneralize env t

minifyVarsFunc :: (VarNames a) => a -> TVarName -> TVarName
minifyVarsFunc xs n = fromMaybe n $ Map.lookup n vars
  where vars = Map.fromList $ zip (Set.toList $ freeTypeVars xs) ([0..] :: [TVarName])

minifyVars :: (VarNames a) => a -> a
minifyVars xs = mapVarNames (minifyVarsFunc xs) xs

getVarInstances :: Infer (Map.Map TVarName (Set.Set QualType))
getVarInstances = varInstances <$> get


getMainSubst :: Infer TSubst
getMainSubst = mainSubst <$> get

applyMainSubst :: Substable b => b -> Infer b
applyMainSubst x =
  do s <- getMainSubst
     return $ applySubst s x


substVar :: TSubst -> TVarName -> TVarName
substVar subst x = let varX = Fix (TBody (TVar x))
                   in case applySubst subst varX of
                          Fix (TBody (TVar zx)) -> zx
                          _ -> x

lookupClass :: ClassName -> Infer (Maybe (Class Type))
lookupClass cs = Map.lookup cs . classes <$> get
