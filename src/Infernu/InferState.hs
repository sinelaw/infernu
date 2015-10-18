{-# LANGUAGE CPP           #-}
{-# LANGUAGE TupleSections #-}

module Infernu.InferState
       ( Infer
       , addPendingUnification
       , addVarScheme
       , applyMainSubst
       , applySubstInfer
       , emptyInferState
       , failWith
       , failWithM
       , fresh
       , freshFlex
       , freshVarId
       , generalize
       , getMainSubst
       , getNamedType
       , getPendingUnifications
       , getState
       , getVarId
       , getVarScheme
       , instantiate
       , instantiateVar
       , instantiateScheme
       , lookupClass
       , mapError
       , minifyVars
       , minifyVarsFunc
       , runInfer
       , runInferWith
       , runSubInfer
       , setPendingUnifications
       , setState
       , setVarScheme
       , skolemiseScheme
       , throwError
       , unrollName
       , wrapError
--             , getVarScheme
         )
       where


import           Control.Monad (foldM, forM, liftM2)
import           Control.Monad.Trans (lift)
import           Control.Monad.Trans.Either (EitherT (..), bimapEitherT, left, runEitherT)
import           Control.Monad.Trans.State (StateT (..), evalStateT, get, mapStateT, modify, put)
import           Data.Foldable (msum)

import           Data.Functor.Identity (Identity (..), runIdentity)
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe)
import           Data.Set (Set)
import qualified Data.Set as Set

import           Text.PrettyPrint.ANSI.Leijen (Doc, Pretty (..), align, squotes, text, (<+>))

import qualified Infernu.Builtins.TypeClasses
import           Infernu.Lib
import           Infernu.Log
import           Infernu.Prelude
import           Infernu.Pretty ()
import           Infernu.Expr (Exp(..), EVarName)
import           Infernu.Source (Source(..), TypeError(..))
import           Infernu.Types

-- | Inference monad. Used as a stateful context for generating fresh type variable names.
type Infer a = StateT InferState (EitherT TypeError Identity) a

emptyInferState :: InferState
emptyInferState = InferState { nameSource = NameSource 2
                             , mainSubst = nullSubst
                             , varSchemes = Map.empty
                             , namedTypes = Map.empty
                             , pendingUni = Set.empty
                             , classes = Map.fromList Infernu.Builtins.TypeClasses.typeClasses
                             }

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

fresh :: Infer Int
fresh = do
  modify $ \is -> is { nameSource = (nameSource is) { lastName = lastName (nameSource is) + 1 } }
  lastName . nameSource <$> get

freshFlex :: Kind -> Infer TVarName
freshFlex k = flip Flex k <$> fresh

freshVarId :: Infer VarId
freshVarId = VarId <$> fresh

throwError :: Source -> Doc -> Infer a
throwError p s = lift . left $ TypeError p s

failWith :: Maybe a -> Infer a -> Infer a
failWith action err = case action of
                          Nothing -> err
                          Just x -> return x

failWithM :: Infer (Maybe a) -> Infer a -> Infer a
failWithM action err = do
  result <- action
  failWith result err

mapError :: (TypeError -> TypeError) -> Infer a -> Infer a
mapError f = mapStateT (bimapEitherT f id)

wrapError :: (TypeError -> Doc) -> Source -> Infer a -> Infer a
wrapError f s = mapError
                $ \te -> TypeError { source = s,
                                     message = f te
                                   }


getVarSchemeByVarId :: VarId -> Infer (Maybe TypeScheme)
getVarSchemeByVarId varId = Map.lookup varId . varSchemes <$> get

getVarId :: EVarName -> TypeEnv -> Maybe VarId
getVarId = Map.lookup

getVarScheme :: Source -> EVarName -> TypeEnv -> Infer (Maybe TypeScheme)
getVarScheme a n env = case getVarId n env of
                       Nothing -> throwError a $ text "Unbound variable:" <+> squotes (pretty n)
                       Just varId -> getVarSchemeByVarId varId

setVarScheme :: TypeEnv -> EVarName -> TypeScheme -> VarId -> Infer TypeEnv
setVarScheme env n scheme varId = do
    traceLog $ text "Inserting scheme for " <+> pretty n <+> text ": " <+> pretty scheme
    modify $ \is -> is { varSchemes = Map.insert varId scheme $ varSchemes is }
    return $ Map.insert n varId env

addVarScheme :: TypeEnv -> EVarName -> TypeScheme -> Infer TypeEnv
addVarScheme env n scheme = do
  varId <- tracePretty (text "-- '" <+> pretty n <+> text "' = varId") <$> freshVarId
  setVarScheme env n scheme varId

--addPendingUnification :: (Source, Type, (ClasSet TypeScheme) -> Infer ()
addPendingUnification :: (Source, Type, (ClassName, Set TypeScheme)) -> Infer ()
addPendingUnification ts = do
    modify $ \is -> is { pendingUni = Set.insert ts $ pendingUni is }
    return ()

getPendingUnifications :: Infer (Set (Source, Type, (ClassName, Set TypeScheme)))
getPendingUnifications = pendingUni <$> get

setPendingUnifications :: Set (Source, Type, (ClassName, Set TypeScheme)) -> Infer ()
setPendingUnifications ts = do
    modify $ \is -> is { pendingUni = ts }
    return ()


----------------------------------------------------------------------


getFreeTVars :: TypeEnv -> Infer (Set TVarName)
getFreeTVars env = do
  let collectFreeTVs s varId = Set.union s <$> curFreeTVs
          where curFreeTVs = tr . maybe Set.empty freeTypeVars <$> getVarSchemeByVarId varId
                tr = tracePretty $ text " collected from" <+> pretty varId <+> text "free type variables:"
  foldM collectFreeTVs Set.empty (Map.elems env)

addNamedType :: TypeId -> Type -> TypeScheme -> Infer ()
addNamedType tid t scheme = do
  traceLog $ text "===> Introducing named type:" <+> pretty tid <+> text "=>" <+> align (pretty scheme)
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
areEquivalentNamedTypes :: (Type, TypeScheme) -> (Type, TypeScheme) -> Bool
areEquivalentNamedTypes (t1, s1) (t2, s2) =
    case matchZip (schemeVars s1) (schemeVars s2) of
        Just zipvars -> s2 == (s2 { schemeType = applySubst subst' $ replaceFixQual (unFix t1) (unFix t2) $ schemeType s1 })
            where subst' = foldr accumSubst' nullSubst zipvars
                  accumSubst' (x,y) s = singletonSubst x (Fix $ TBody $ TVar y) `composeSubst` s
        Nothing -> False

-- | Returns a TQual with the `src` type replaced everywhere with the `dest` type.
replaceFixQual :: (Functor f, Eq (f (Fix f))) => f (Fix f) -> f (Fix f) -> TQual (Fix f) -> TQual (Fix f)
replaceFixQual src dest (TQual preds t) = TQual (map (replacePredType' $ replaceFix src dest) preds) (replaceFix src dest t)
    where replacePredType' f p = p { predType = f $ predType p } -- TODO needs some lens goodness

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
   TCons (TName typeId' k) subTs -> recurseIntoNamedType typeId' subTs
   TCons _ subTs -> msum $ map (isRecParamOnly n1 Nothing) subTs
   TFunc ts tres -> isRecParamOnly n1 Nothing tres `mappend` msum (map (isRecParamOnly n1 Nothing) ts)
   TRow _ rlist -> isRecParamRecList n1 rlist
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

dropKindPrefix :: Int -> Kind -> Kind
dropKindPrefix 0 k = k
dropKindPrefix n (KArrow _ k) = dropKindPrefix (n - 1) k
dropKindPrefix n k = error . show $ text "Can't drop" <+> pretty n <+> text "from kind" <+> pretty k

replaceRecType :: TypeId -> TypeId -> Int -> Type -> Type
replaceRecType typeId newTypeId indexToDrop t1 =
    let replace' = replaceRecType typeId newTypeId indexToDrop
        mapTs' = map replace'
    in  case unFix t1 of
            TBody _ -> t1
            TCons (TName typeId' k) subTs -> if typeId == typeId'
                                          then Fix $ TCons (TName newTypeId $ dropKindPrefix indexToDrop k) $ dropAt indexToDrop subTs
                                          else t1
            TCons n subTs -> Fix $ TCons n $ mapTs' subTs
            TFunc ts tres -> Fix $ TFunc (mapTs' ts) (replace' tres)
            TRow l rlist -> record l $ go rlist
             where go rlist' =
                     case rlist' of
                      TRowEnd _ -> rlist'
                      TRowProp p (TScheme qv t') rlist'' -> TRowProp p (TScheme qv (t' { qualType = replace' $ qualType t' })) (go rlist'')
                      TRowRec tid ts -> if typeId == tid
                                        then TRowRec newTypeId $ dropAt indexToDrop ts
                                        else rlist'

allocNamedType :: Source -> TVarName -> Kind -> Type -> Infer Type
allocNamedType a n k t =
  do typeId <- TypeId <$> fresh
     let namedTypeParams = map (Fix . TBody . TVar) . Set.toList $ freeTypeVars t `Set.difference` Set.singleton n
         namedTypeKind = karrow k $ map kind namedTypeParams
         namedType = Fix $ TCons (TName typeId namedTypeKind) namedTypeParams
         target = applySubst (singletonSubst n namedType) t

     traceLog $ text "===> replacing" <+> pretty n <+> text "=>" <+> align (pretty namedType)
     traceLog $ text "===> allocNamedType: source =" <+> align (pretty t)
     traceLog $ text "===> allocNamedType: target =" <+> align (pretty target)
     (scheme, ps) <- unsafeGeneralize Map.empty $ qualEmpty target
     traceLog $ text "===> Generated scheme for mu type:" <+> align (pretty scheme)
     currentNamedTypes <- filter (\(_, ts) -> areEquivalentNamedTypes (namedType, scheme) ts) . Map.toList . namedTypes <$> get
     case currentNamedTypes of
      [] -> do traceLog $ text "===> Didn't find existing rec type, adding new: " <+> pretty namedType <+> text "=" <+> align (pretty scheme)
               addNamedType typeId namedType scheme
               return namedType
      (otherTID, (otherNT, _)):_ ->
          do traceLog $ text "===> Found existing rec type:" <+> pretty otherNT
             -- TODO: don't ignore the preds!
             qualType <$> unrollName a otherTID namedTypeParams

resolveSimpleMutualRecursion :: TVarName -> Type -> TypeId -> Int -> Infer Type
resolveSimpleMutualRecursion n t tid ix =
  do (Fix (TCons (TName _ k) ts), scheme) <- (Map.lookup tid . namedTypes <$> get) `failWithM` error "oh no." -- TODO
     newTypeId <- TypeId <$> fresh
     let qVars' = dropAt ix $ schemeVars scheme
         replaceOldNamedType = replaceRecType tid newTypeId ix
         sType' = (schemeType scheme) { qualType = replaceOldNamedType $ qualType $  schemeType scheme }
         newTs = dropAt ix ts
         newNamedType = Fix (TCons (TName newTypeId $ dropKindPrefix ix k) newTs)
         --updatedNamedType = Fix (TCons (TName tid) newTs)
         updatedScheme = applySubst (singletonSubst n newNamedType) $ TScheme qVars'  sType'

     addNamedType newTypeId newNamedType updatedScheme
     -- TODO: we could alternatively update the existing named type, but that will break it's schema (will now take less params?)
     --addNamedType tid updatedNamedType updatedScheme
     return $ replaceOldNamedType t


getNamedType :: Source -> TVarName -> Kind -> Type -> Infer Type
getNamedType a n k t =
  do let recTypeParamPos = isRecParamOnly n Nothing t
     traceLog $ text "isRecParamOnly:" <+> pretty n <+> text "in" <+> pretty t <+> text ":" <+> align (text $ show (fmap show recTypeParamPos))
     case recTypeParamPos of
      Just [(tid, ix)] -> resolveSimpleMutualRecursion n t tid ix
      -- either the variable appears outside a recursive type's type parameter list, or it appears
      -- in more than one such position:
      _ -> applyMainSubst t >>= allocNamedType a n k


unrollNameByScheme :: Substable a => [Type] -> [TVarName] -> a -> a
unrollNameByScheme ts qvars = applySubst subst
  where assocs = case matchZip qvars ts of
                     Just x -> x
                     Nothing -> error "Assertion failed: Expected number of types = number of tvars when unrolling a recursive type."
        subst = foldr (\(tvar,destType) s -> singletonSubst tvar destType `composeSubst` s) nullSubst assocs

getNamedTypeScheme :: Source -> TypeId -> Infer TypeScheme
getNamedTypeScheme a tid = (fmap snd . Map.lookup tid . namedTypes <$> get) `failWithM` throwError a (text "Unknown type id")

-- | Unrolls (expands) a TName recursive type by plugging in the holes from the given list of types.
-- Similar to instantiation, but uses a pre-defined set of type instances instead of using fresh
-- type variables.
unrollName :: Source -> TypeId -> [Type] -> Infer QualType
unrollName a tid ts =
    -- TODO: Is it safe to ignore the scheme preds here?
    do (TScheme qvars t) <- getNamedTypeScheme a tid
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
  do traceLog $ text "applying subst: " <+> align (pretty s)
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
instantiateScheme :: TypeScheme -> Infer QualType
instantiateScheme (TScheme tvarNames t) = do
  allocNames <- allocTVarInstances tvarNames
  let replaceVar n = fromMaybe n $ lookup n allocNames
  return $ mapVarNames replaceVar t

allocTVarInstances :: [TVarName] -> Infer [(TVarName, TVarName)]
allocTVarInstances tvarNames = forM tvarNames $ \tvName -> (tvName,) . flip Flex (kind tvName) <$> fresh

instantiate :: TypeScheme -> Infer QualType
instantiate = instantiateScheme

instantiateVar :: Source -> EVarName -> TypeEnv -> Infer QualType
instantiateVar a n env = do
  varId <- getVarId n env `failWith` throwError a (text "Unbound variable:" <+> squotes (pretty n))
  scheme <- getVarSchemeByVarId varId `failWithM` throwError a (text "Assertion failed: missing var scheme for:" <+> squotes (pretty n))
  tracePretty (text "Instantiated var '" <+> pretty n <+> text "' with scheme: " <+> pretty scheme <+> text "to") <$> instantiate scheme

----------------------------------------------------------------------

-- Performs deep skolemisation, retuning the skolem constants and the skolemised type
skolemiseScheme :: TypeScheme -> Infer ([TVarName], QualType)
skolemiseScheme (TScheme tvs ty)
  = do sks1 <- forM tvs $ \tv -> (flip Skolem (kind tv) <$> fresh)
       let subst = Map.fromList $ zip tvs sks1
           lookup' x = fromMaybe x $ Map.lookup x subst
       (sks2, ty') <- skolemiseType (mapVarNames lookup' ty)
       return (sks1 ++ sks2, ty')

skolemiseType :: QualType -> Infer ([TVarName], QualType)
skolemiseType q@(TQual ps (Fix (TFunc args_ty res_ty)))
  = do (sks, TQual ps' res_ty'') <- skolemiseType (qualEmpty res_ty)
       return (sks, q { qualPred = ps ++ ps',  qualType = Fix $ TFunc args_ty res_ty'' })
skolemiseType ty
  = return ([], ty)

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
unsafeGeneralize :: TypeEnv -> QualType -> Infer (TypeScheme, [TPred Type])
unsafeGeneralize tenv t = do
    traceLog $ text "Generalizing: " <+> pretty t
    s <- getMainSubst
    let t' = applySubst s t
        ftvs = freeTypeVars t'
    unboundVars <- Set.difference ftvs <$> getFreeTVars tenv
    traceLog $ text "Generalization result: unbound vars =" <+> pretty unboundVars <+> text ", type =" <+> pretty t'
    let (qvarPreds, floatedPreds) = List.partition (\p -> Set.null $ freeTypeVars p `Set.intersection` unboundVars) $ qualPred t'
        t'' = TQual { qualPred = floatedPreds, qualType = qualType t' }

    return (TScheme (Set.toList unboundVars) t'', floatedPreds)

isExpansive :: Exp a -> Bool
isExpansive (EVar{})    = False
isExpansive (EAbs{})    = False
isExpansive (ELit{})    = False
isExpansive (ECase _ ep es) = any isExpansive (ep:map snd es)
isExpansive (EProp _ e _)   = isExpansive e
isExpansive (EApp{})    = True
isExpansive (EPropAssign{}) = True
isExpansive (ELet{})    = True
isExpansive (EArray{})  = True
isExpansive (ETuple{})  = True
isExpansive (EStringMap{})  = True
isExpansive (ERow{})        = True
isExpansive (ENew{})        = True


generalize :: Exp a -> TypeEnv -> QualType -> Infer (TypeScheme, [TPred Type])
generalize exp' env t = if isExpansive exp'
                        then return (TScheme [] t, [])
                        else unsafeGeneralize env t

minifyVarsFunc :: (VarNames a) => a -> TVarName -> TVarName
minifyVarsFunc xs n = maybe n (setTVarName n) $ Map.lookup n vars
  where vars = Map.fromList $ zip (Set.toList $ freeTypeVars xs) [0..]

minifyVars :: (VarNames a) => a -> a
minifyVars xs = mapVarNames (minifyVarsFunc xs) xs

getMainSubst :: Infer TSubst
getMainSubst = mainSubst <$> get

applyMainSubst :: Substable b => b -> Infer b
applyMainSubst x =
  do s <- getMainSubst
     return $ applySubst s x


lookupClass :: ClassName -> Infer (Maybe (Class Type))
lookupClass cs = Map.lookup cs . classes <$> get
