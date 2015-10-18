{-# LANGUAGE CPP           #-}
{-# LANGUAGE TupleSections #-}

module Infernu.Unify
       (unify, unifyAll, unifyl, unifyTypeSchemes, unifyPredsL, unifyPending, tryMakeRow)
       where


import           Control.Monad        (forM, forM_, when, unless)

import           Data.Either          (rights)
import           Data.Map.Strict        (Map)
import qualified Data.Map.Strict        as Map
import           Data.Maybe           (catMaybes, mapMaybe)

import           Data.Set             (Set)
import qualified Data.Set             as Set

import           Text.PrettyPrint.ANSI.Leijen (Pretty (..), align, text, (<+>), vsep, align, indent, empty, parens, Doc)
import qualified Infernu.Builtins as Builtins
import           Infernu.Prelude
import           Infernu.Decycle
import           Infernu.InferState
import           Infernu.Lib          (matchZip)
import           Infernu.Log
import           Infernu.Pretty()
import           Infernu.Types
import           Infernu.Source       (Source(..), TypeError(..))
import           Infernu.Expr         (EPropName(..))

----------------------------------------------------------------------

tryMakeRow :: FType Type -> Infer (Maybe (TRowList Type))
tryMakeRow (TCons TStringMap [t]) = Just <$> Builtins.stringMapRowType t
tryMakeRow (TCons TArray [t]) = Just <$> Builtins.arrayRowType t
tryMakeRow (TCons TRecord [Fix (TRow _ rl)]) = Just <$> return rl
tryMakeRow (TBody TRegex) = Just <$> Builtins.regexRowType
tryMakeRow (TBody TString) = Just <$> Builtins.stringRowType
tryMakeRow (TBody TDate) = Just <$> Builtins.dateRowType
tryMakeRow (TRow _ rl) = Just <$> return rl
tryMakeRow (TFunc targs tres) = Just <$> (return
                                          . TRowProp (TPropGetName EPropFun) (schemeEmpty $ Fix $ TFunc targs tres)
                                          $ TRowEnd Nothing)

tryMakeRow _ = return Nothing

----------------------------------------------------------------------


type UnifyF = Source -> Type -> Type -> Infer ()

unify :: UnifyF
unify = decycledUnify

-- | Unifies given types, using the namedTypes from the infer state
-- >>> let p = emptySource
-- >>> let u x y = runInfer $ unify p x y >> getMainSubst
-- >>> let du x y = unify p x y >> getMainSubst
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
-- >>> u (Fix $ TBody $ TVar 0) (Fix $ TRow $ TRowProp "x" (schemeEmpty $ Fix $ TBody TNumber) (TRowEnd $ Just $ RowTVar 1))
-- Right (fromList [(0,Fix (TRow (TRowProp "x" (TScheme {schemeVars = [], schemeType = TQual {qualPred = [], qualType = Fix (TBody TNumber)}}) (TRowEnd (Just (RowTVar 1))))))])
--
-- >>> let row1 z = (Fix $ TRow $ TRowProp "x" (schemeEmpty $ Fix $ TBody TNumber) (TRowEnd z))
-- >>> let sCloseRow = fromRight $ u (row1 $ Just $ RowTVar 1) (row1 Nothing)
-- >>> pretty $ applySubst sCloseRow (row1 $ Just $ RowTVar 1)
-- "{x: Number}"
--
-- Simple recursive type:
--
-- >>> let tvar0 = Fix $ TBody $ TVar 0
-- >>> let tvar3 = Fix $ TBody $ TVar 3
-- >>> let recRow = Fix $ TRow $ TRowProp "x" (schemeEmpty tvar0) $ TRowProp "y" (schemeEmpty tvar3) (TRowEnd $ Just $ RowTVar 2)
-- >>> let s = fromRight $ u tvar0 recRow
-- >>> s
-- fromList [(0,Fix (TCons (TName (TypeId 1)) [Fix (TBody (TVar 2)),Fix (TBody (TVar 3))]))]
-- >>> applySubst s tvar0
-- Fix (TCons (TName (TypeId 1)) [Fix (TBody (TVar 2)),Fix (TBody (TVar 3))])
--
-- >>> :{
-- pretty $ runInfer $ do
--     s <- du tvar0 recRow
--     let (Fix (TCons (TName n1) targs1)) = applySubst s tvar0
--     t <- unrollName p n1 targs1
--     return t
-- :}
-- "{x: <Named Type: mu 'B'. c d>, y: d, ..c}"
--
-- Unifying a rolled recursive type with its (unequal) unrolling should yield a null subst:
--
-- >>> :{
-- runInfer $ do
--     s <-  du tvar0 recRow
--     let rolledT = applySubst s tvar0
--     let (Fix (TCons (TName n1) targs1)) = rolledT
--     unrolledT <- unrollName p n1 targs1
--     du rolledT unrolledT
--     return (rolledT == unrolledT)
-- :}
-- Right False
--
-- >>> :{
-- pretty $ runInfer $ do
--     du tvar0 recRow
--     let tvar4 = Fix . TBody . TVar $ 4
--         tvar5 = Fix . TBody . TVar $ 5
--     s2 <- du recRow (Fix $ TRow $ TRowProp "x" (schemeEmpty tvar4) $ TRowProp "y" (schemeEmpty tvar5) (TRowEnd Nothing))
--     return $ applySubst s2 recRow
-- :}
-- "{x: <Named Type: mu 'B'. {} f>, y: f}"
--
-- >>> let rec2 = Fix $ TCons TFunc [recRow, Fix $ TBody TNumber]
-- >>> :{
-- pretty $ runInfer $ do
--     s1 <- du tvar0 rec2
--     return $ applySubst s1 $ qualEmpty rec2
-- :}
-- "(this: {x: <Named Type: mu 'B'. c d>, y: d, ..c} -> TNumber)"
--
-- >>> :{
-- runInfer $ do
--     s1 <- du tvar0 rec2
--     s2 <- du tvar0 rec2
--     return $ (applySubst s1 (qualEmpty rec2) == applySubst s2 (qualEmpty rec2))
-- :}
-- Right True
--
--
-- Test generalization/instantiation of recursive types
--
-- >>> :{
-- pretty $ runInfer $ do
--     s1 <- du tvar0 rec2
--     generalize (ELit "bla" LitUndefined) Map.empty $ applySubst s1 $ qualEmpty rec2
-- :}
-- "forall c d. (this: {x: <Named Type: mu 'B'. c d>, y: d, ..c} -> TNumber)"
--
-- >>> :{
-- putStrLn $ fromRight $ runInfer $ do
--     s1 <- du tvar0 rec2
--     tscheme <- generalize (ELit "bla" LitUndefined) Map.empty $ applySubst s1 $ qualEmpty rec2
--     Control.Monad.forM_ [1,2..10] $ const fresh
--     t1 <- instantiate tscheme
--     t2 <- instantiate tscheme
--     unrolledT1 <- unrollName p (TypeId 1) [Fix $ TRow $ TRowEnd Nothing]
--     return $ concat $ Data.List.intersperse "\n"
--                           [ pretty tscheme
--                           , pretty t1
--                           , pretty t2
--                           , pretty unrolledT1
--                           ]
-- :}
-- forall c d. (this: {x: <Named Type: mu 'B'. c d>, y: d, ..c} -> TNumber)
-- (this: {x: <Named Type: mu 'B'. m n>, y: n, ..m} -> TNumber)
-- (this: {x: <Named Type: mu 'B'. o p>, y: p, ..o} -> TNumber)
-- (this: {x: <Named Type: mu 'B'. {} d>, y: d} -> TNumber)
--
--
decycledUnify :: UnifyF
decycledUnify = decycle3 unify''

unlessEq :: (Monad m, Eq a) => a -> a -> m () -> m ()
unlessEq x y = unless (x == y)

wrapWithUnifyError :: Pretty a => a -> a -> Maybe TypeError -> Doc
wrapWithUnifyError t1 t2 =
    mkTypeErrorMessage msg
    where msg = vsep [text "Failed unifying:"
                     , indent 4 $ pretty t1
                     , text "With:"
                     , indent 4 $ pretty t2
                     ]

mkTypeErrorMessage :: Doc -> Maybe TypeError -> Doc
mkTypeErrorMessage msg mte =
    vsep [ msg
         , case mte of
               Nothing -> empty
                         --   "             With:  "
               Just te -> vsep [text "Because:", message te]
         ]

wrapError' :: Pretty b => Source -> b -> b -> Infer a -> Infer a
wrapError' s ta tb = wrapError (wrapWithUnifyError ta tb . Just) s

unify'' :: Maybe UnifyF -> UnifyF
unify'' Nothing _ t1 t2 = traceLog $ text "breaking infinite recursion cycle, when unifying: " <+> pretty t1 <+> text " ~ " <+> pretty t2
unify'' (Just recurse) a t1 t2 =
  do traceLog $ text "unifying: " <+> pretty t1 <+> text " ~ " <+> pretty t2
     unlessEq t1 t2 $ do
         traceLog $ text "They are not equal"
         unless (kind t1 == kind t2) $ wrapError' a t1 t2 $ throwError a $ text "Can't unify, mismatching kinds:" <+> pretty (kind t1) <+> text "!=" <+> pretty (kind t2)
         s <- getMainSubst
         let t1' = unFix $ applySubst s t1
             t2' = unFix $ applySubst s t2
         traceLog $ text "unifying (substed): " <+> pretty t1 <+> text " ~ " <+> pretty t2
         wrapError' a t1 t2 $ unify' recurse a t1' t2'

unificationError :: (VarNames x, Pretty x) => Source -> x -> x -> Infer b
unificationError pos x y = throwError pos $ wrapWithUnifyError a b Nothing
  where [a, b] = minifyVars [x, y]

assertNoPred :: QualType -> Infer Type
assertNoPred q =
    do  unless (null $ qualPred q) $ fail $ show $ text "Assertion failed: pred in " <+> pretty q
        return $ qualType q

-- | Main unification function
unify' :: UnifyF -> Source -> FType (Fix FType) -> FType (Fix FType) -> Infer ()

-- | Type variables
unify' _ a (TBody (TVar (Flex n k))) t = varBind a (Flex n k) (Fix t)
unify' _ a t (TBody (TVar (Flex n k))) = varBind a (Flex n k) (Fix t)

-- | Skolem type "variables"
unify' _ a t1@(TBody (TVar (Skolem n1 k1))) t2@(TBody (TVar (Skolem n2 k2)))
    | (n1 == n2) && (k1 == k2) = return ()
    | otherwise =  unificationError a t1 t2
unify' _ a t1 t2@(TBody (TVar (Skolem _ _))) = unificationError a t1 t2
unify' _ a t1@(TBody (TVar (Skolem _ _))) t2 = unificationError a t1 t2

-- | TEmptyThis <- something
unify' _ _ (TBody TEmptyThis) _ = return ()

-- | TUndefined <- TEmptyThis
unify' _ _ (TBody TUndefined) (TBody TEmptyThis) = return ()

-- | Two simple types
unify' _ a (TBody x) (TBody y) = unlessEq x y $ unificationError a x y

-- | Two recursive types
unify' recurse a t1@(TCons (TName n1 k1) targs1) t2@(TCons (TName n2 k2) targs2) =
    -- TODO check kinds
    if n1 == n2
    then case matchZip targs1 targs2 of
             Nothing -> unificationError a t1 t2
             Just targs -> unifyl recurse a targs
    else
        do let unroll' = unrollName a
           t1' <- unroll' n1 targs1
           t2' <- unroll' n2 targs2
           -- TODO don't ignore qual preds...
           mapM_ assertNoPred [t1', t2']
           recurse a (qualType t1') (qualType t2')

-- | A recursive type and another type
unify' recurse a (TCons (TName n1 k) targs1) t2 =
    unrollName a n1 targs1
    >>= assertNoPred
    >>= flip (recurse a) (Fix t2)
unify' recurse a t1 (TCons (TName n2 k) targs2) =
    unrollName a n2 targs2
    >>= assertNoPred
    >>= recurse a (Fix t1)

-- | Two type constructors
-- | Any others
unify' recurse a t1@(TCons n1 ts1) t2@(TCons n2 ts2)
    | (n1 == n2) = case matchZip ts1 ts2 of
        Nothing -> unificationError a t1 t2
        Just ts -> unifyl recurse a ts
    | (n1 == TRecord) || (n2 == TRecord) = unifyTryMakeRow recurse a t1 t2
    | otherwise = unificationError a t1 t2

-- | Two functions
unify' recurse a t1@(TFunc ts1 tres1) t2@(TFunc ts2 tres2) =
    case matchZip ts2 ts1 of
        Nothing -> unificationError a t1 t2
        Just ts -> do  unifyl recurse a ts
                       recurse a tres1 tres2

-- | A type constructor vs. a simple type
unify' r a t1@(TBody{}) t2@(TCons{}) = unifyTryMakeRow r a t1 t2
unify' r a t1@(TCons{}) t2@(TBody{}) = unifyTryMakeRow r a t1 t2

-- | A function vs. a simple type
unify' r a t1@(TBody{}) t2@(TFunc{}) = unifyTryMakeRow r a t1 t2
unify' r a t1@(TFunc{}) t2@(TBody{}) = unifyTryMakeRow r a t1 t2

-- | A function vs. a type constructor
unify' r a t1@(TFunc{}) t2@(TCons{}) = unifyTryMakeRow r a t1 t2
unify' r a t1@(TCons{}) t2@(TFunc{}) = unifyTryMakeRow r a t1 t2

-- | Type constructor vs. row type
unify' r a t1@(TRow{})  t2@(TCons{}) = unifyTryMakeRow r a t1 t2
unify' r a t1@(TCons{}) t2@(TRow{})  = unifyTryMakeRow r a t1 t2
unify' r a t1@(TRow{})  t2@(TBody{}) = unifyTryMakeRow r a t1 t2
unify' r a t1@(TBody{}) t2@(TRow{})  = unifyTryMakeRow r a t1 t2
unify' r a t1@(TRow{})  t2@(TFunc{}) = unifyTryMakeRow r a t1 t2
--unify' r a t1@(TFunc{}) t2@(TRow{}) = unifyTryMakeRow r a t1 t2
unify' r a t1@(TFunc{}) t2@(TRow  _ rl) =
    case Map.lookup (TPropGetName EPropFun) . fst $ flattenRow rl of
        Nothing -> unificationError a t1 t2
        Just ts -> do t2' <- instantiate ts
                      r a (Fix t1) $ qualType t2'




-- | Two row types
-- TODO: un-hackify!
unify' recurse a t1@(TRow _ row1) t2@(TRow _ row2) =
  unlessEq t1 t2 $ do
     let (m2, r2) = flattenRow row2
         names2List = Map.keys m2
         names2 = Set.fromList names2List
         (m1, r1) = flattenRow row1
         names1List = Map.keys m1
         names1 = Set.fromList names1List
         -- TODO: order of x,y should depend on get vs. set to ensure variance is handled correctly
         commonNames = [(x,y) | x <- names1List, y <- names2List, tpropName x == tpropName y]

         --namesToTypes :: Map EPropName (TScheme t) -> [EPropName] -> [t]
         -- TODO: This ignores quantified variables in the schemes.
         -- It should be AT LEAST alpha-equivalence below (in the unifyl)
         namesToTypes m = mapMaybe $ flip Map.lookup m
         --commonTypes :: [(Type, Type)]
         commonTypes = zip (namesToTypes m1 $ map fst commonNames) (namesToTypes m2 $ map snd commonNames)


     traceLog $ text "row1: " <+> pretty m1 <+> text ", " <+> pretty r1
     traceLog $ text "row2: " <+> pretty m2 <+> text ", " <+> pretty r2
     traceLog $ text "Common row properties: " <+> pretty commonNames
     forM_ commonTypes $ \(ts1, ts2) -> wrapError' a ts1 ts2 $ unifyTypeSchemes' recurse a ts1 ts2

     let allAreCommon = Set.null $ (names1 `Set.difference` names2) `Set.union` (names2 `Set.difference` names1)
         unifyDifferences =
             do  r <- RowTVar . (flip Flex KRow) <$> fresh
                 let flippedRecurse a' = flip $ recurse a'
                 unifyRows        recurse a r (t1, names1, m1) (t2, names2, r2)
                 unifyRows flippedRecurse a r (t2, names2, m2) (t1, names1, r1)
         unifyRowTVars act =
             case (r1, r2) of
                 (FlatRowEndTVar (Just r1v), FlatRowEndTVar (Just r2v)) -> recurse a (toRowTVar r1v) (toRowTVar r2v)
                     where toRowTVar = Fix . TBody . TVar . getRowTVar
                 _ -> act
     if allAreCommon
         then unifyRowTVars unifyDifferences
         else unifyDifferences

unifyTryMakeRow :: UnifyF -> Source -> FType Type -> FType Type -> Infer ()
unifyTryMakeRow r a t1 t2 =
  do res1 <- tryMakeRow t1
     res2 <- tryMakeRow t2
     case (res1, res2) of
      (Just rowType1, Just rowType2) -> r a t1' t2'
         where t1' = record (label t1) rowType1
               t2' = record (label t2) rowType2
               label t = case t of
                            TCons cons _ -> Just $ show $ pretty cons
                            TRow l _ -> l
                            _ -> Just $ show $ pretty $ Fix t
      _ -> wrapError' a t1 t2 $ throwError a
           $ text "Failed tryMakeRow unification:" <+> align (vsep [pretty res1, text "with", pretty res2])


unifyTypeSchemes :: Source -> TypeScheme -> TypeScheme -> Infer ()
unifyTypeSchemes = unifyTypeSchemes' unify

-- | Biased subsumption-based unification. Succeeds if scheme2 is at least as polymorphic as scheme1
unifyTypeSchemes' :: UnifyF -> Source -> TypeScheme -> TypeScheme -> Infer ()
unifyTypeSchemes' recurse a scheme1s scheme2s =
   do traceLog $ text "Unifying type schemes: " <+> pretty scheme1s <+> text " ~ " <+> pretty scheme2s

      (skolemVars, scheme1T) <- skolemiseScheme scheme1s
      scheme2T <- instantiate scheme2s

      traceLog $ text "Instantiated skolems: " <+> align (vsep [pretty scheme1T, pretty scheme2T])
      traceLog $ indent 4 $ text "skolems:" <+> pretty skolemVars

      recurse a (qualType scheme1T) (qualType scheme2T)
      let isSkolem (Fix (TBody (TVar (Skolem _ _)))) = True
          isSkolem _ = False
          oldSkolems = concatMap (filter isSkolem . map (Fix . TBody . TVar) . Set.toList . freeTypeVars) [scheme1s, scheme2s]
      ftvs <- mapM (applyMainSubst . map (Fix . TBody . TVar) . Set.toList . freeTypeVars) [scheme1s, scheme2s]
      let escapedSkolems = filter (\x -> isSkolem x && x `notElem` oldSkolems) $ concat ftvs

      unless (null escapedSkolems)
          $ throwError a $ vsep [ text "Subsumption failed, type:"
                                , indent 4 $ pretty scheme2s
                                , text "is not as polymorphic as:"
                                , indent 4 $ pretty scheme1s
                                , parens $ text "escaped skolems: " <+> pretty escapedSkolems
                                ]

      -- preds
      preds1' <- qualPred <$> applyMainSubst scheme1T
      preds2' <- qualPred <$> applyMainSubst scheme2T
      -- TODO what to do with the ambiguous preds?
      traceLog $ text "Checking entailment of: \n\t" <+> pretty preds1' <+> text "\n  from:\n\t" <+> pretty preds2'
      let preds1Set = Set.fromList preds1'
          preds2Set = Set.fromList preds2'
          symDiff s1 s2 = (s1 `Set.difference` s2) `Set.union` (s2 `Set.difference` s1)
      -- TODO this will fail wrongly if the remaining preds contain skolems
      ambiguousPreds <- unifyPredsL a $ Set.toList $ symDiff preds1Set preds2Set
      return ()

unifyRows :: (VarNames x, Pretty x) => UnifyF -> Source -> RowTVar
               -> (x, Set TProp, Map TProp TypeScheme)
               -> (x, Set TProp, FlatRowEnd Type)
               -> Infer ()
unifyRows recurse a r (t1, names1, m1) (t2, names2, r2) =
    do let in1NotIn2 = names1 `Set.difference` names2
           rowTail = case r2 of
                      FlatRowEndTVar (Just _) -> FlatRowEndTVar $ Just r
                      _ -> r2
           in1NotIn2row = Fix . TRow Nothing . unflattenRow m1 rowTail $ flip Set.member in1NotIn2

       traceLog $ text "in1NotIn2row" <+> pretty in1NotIn2row
       traceLog $ text "r2" <+> pretty r2
       case r2 of
         FlatRowEndTVar Nothing -> if Set.null in1NotIn2
                                   then varBind a (getRowTVar r) (Fix $ TRow Nothing $ TRowEnd Nothing)
                                   else throwError a $ vsep [ text "The following properties:"
                                                            , indent 4 $ pretty $ Set.toList in1NotIn2
                                                            , text "are missing from row:"
                                                            , indent 4 $ pretty t2
                                                            ]
         FlatRowEndTVar (Just r2') -> recurse a in1NotIn2row (Fix . TBody . TVar $ getRowTVar r2')
         FlatRowEndRec tid ts -> recurse a in1NotIn2row (Fix $ TCons (TName tid (karrow KStar $ map kind ts)) ts)

-- | Unifies pairs of types, accumulating the substs
unifyl :: UnifyF -> Source -> [(Type, Type)] -> Infer ()
unifyl r a = mapM_ $ uncurry $ r a

-- | Checks if a type var name appears as a free type variable nested somewhere inside a row type.
--
-- >>> getSingleton $ isInsideRowType 0 (Fix (TBody $ TVar 0))
-- Nothing
-- >>> getSingleton $ isInsideRowType 0 (Fix (TRow $ TRowEnd (Just $ RowTVar 0)))
-- Just Fix (TRow (TRowEnd (Just (RowTVar 0))))
-- >>> getSingleton $ isInsideRowType 0 (Fix (TRow $ TRowEnd (Just $ RowTVar 1)))
-- Nothing
-- >>> getSingleton $ isInsideRowType 0 (Fix (TFunc [Fix $ TBody $ TVar 0] (Fix $ TRow $ TRowEnd (Just $ RowTVar 1))))
-- Nothing
-- >>> getSingleton $ isInsideRowType 0 (Fix (TFunc [Fix $ TBody $ TVar 1] (Fix $ TRow $ TRowEnd (Just $ RowTVar 0))))
-- Just Fix (TRow (TRowEnd (Just (RowTVar 0))))
isInsideRowType :: TVarName -> Type -> Set (Maybe String, TRowList Type)
isInsideRowType n (Fix t) =
  case t of
   TRow name t' ->
       if n `Set.member` freeTypeVars t'
       then Set.singleton (name, t')
       else Set.empty
   _ -> foldr (\x l -> isInsideRowType n x `Set.union` l) Set.empty t
--   _ -> unOrBool $ fst (traverse (\x -> (OrBool $ isInsideRowType n x, x)) t)

getSingleton :: Set a -> Maybe a
getSingleton s = case foldr (:) [] s of
                     [x] -> Just x
                     _ -> Nothing

varBind :: Source -> TVarName -> Type -> Infer ()
varBind a n t =
  do s <- varBind' a n t
     case s of
         Nothing -> return ()
         Just s' -> applySubstInfer s'

varBind' :: Source -> TVarName -> Type -> Infer (Maybe TSubst)
varBind' a n t | t == Fix (TBody (TVar n)) = return Nothing
               | Just (rowN, rowList) <- getSingleton $ isInsideRowType n t =
                   do recVar <- flip Flex KStar <$> fresh
                      let rowT = Fix $ TCons TRecord [Fix $ TRow rowN rowList]
                          withRecVar = replaceFix (unFix rowT) (TBody (TVar recVar)) t
                          recT = applySubst (singletonSubst n withRecVar) rowT
                      traceLog $ text "===> Generalizing mu-type: " <+> pretty n <+> text " recursive in: " <+> pretty t <+> text ", found enclosing row type: " <+> text " = " <+> pretty rowT
                      namedType <- getNamedType a recVar KStar recT
                      -- let (TCons (TName n1) targs1) = unFix namedType
                      -- t' <- unrollName a n1 targs1
                      traceLog $ text "===> Resulting mu type: " <+> pretty n <+> text " = " <+> pretty withRecVar
                      return . Just $ singletonSubst recVar namedType `composeSubst` singletonSubst n withRecVar
               | n `Set.member` freeTypeVars t = let f = minifyVarsFunc t
                                                 in throwError a
                                                    $ text "Occurs check failed: "
                                                    <+> pretty (f n) <+> text " in " <+> align (pretty (mapVarNames f t))
               | otherwise = return . Just $ singletonSubst n t

unifyAll :: Source -> [Type] -> Infer ()
unifyAll a ts = unifyl decycledUnify a $ zip ts (drop 1 ts)

-- | Tries to minimize a set of constraints by finding ones that can be unambiguously refined to a
-- specific type. Updates the state (subst) to reflect the found substitutions, and for those
-- constraints that could not be disambiguated - records them as pending disambiguities. Returns the
-- filtered list of yet-to-be-resolved predicates.
unifyPredsL :: Source -> [TPred Type] -> Infer [TPred Type]
unifyPredsL a ps = Set.toList . Set.fromList . catMaybes <$>
    do  forM ps $ \p@(TPredIsIn className t) ->
                  do  entry <- ((a,t,) . (className,) . Set.fromList . classInstances) <$> lookupClass className
                               `failWithM` throwError a (text "Unknown class: " <+> pretty className <+> text "in pred list:" <+> pretty ps)
                      remainingAmbiguities <- unifyAmbiguousEntry entry
                      case remainingAmbiguities of
                          Nothing -> return Nothing
                          Just ambig ->
                              do  addPendingUnification ambig
                                  return $ Just p

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _  = False

catLefts :: [Either a b] -> [a]
catLefts [] = []
catLefts (Left a:xs) = a : catLefts xs
catLefts (Right _:xs) = catLefts xs

-- | Given a type and a set of possible typeclass instances, tries to find an unambiguous
-- unification that works for this type.
--
-- (classname is used for error reporting)
--
-- If none of the possible instances of the typeclass can unify with the given type, fails.
--
-- If there is is more than one possible instance, returns the subset of possible instances.
--
-- If there is exactly one possible instance, applies the unification to the current state and
-- returns Nothing.
--
unifyAmbiguousEntry :: (Source, Type, (ClassName, Set TypeScheme)) -> Infer (Maybe (Source, Type, (ClassName, Set TypeScheme)))
unifyAmbiguousEntry (a, t, (ClassName className, tss)) =
    do  let unifAction ts =
                do inst <- instantiateScheme ts >>= assertNoPred
                   unify a inst t
        unifyResults <- forM (Set.toList tss) $ \instScheme -> (instScheme, ) <$> runSubInfer (unifAction instScheme >> getState)
        let survivors = filter (isRight . snd) unifyResults
        case rights $ map snd survivors of
            []         -> do t' <- applyMainSubst t
                             throwError a $ vsep [ vsep (map message . catLefts $ map snd unifyResults)
                                                 , empty
                                                 , text "While trying to find matching instance of typeclass"
                                                 , indent 4 $ pretty className
                                                 , text "for type:"
                                                 , indent 4 $ pretty t'
                                                 ]
            [newState] -> setState newState >> return Nothing
            _          -> return . Just . (\x -> (a, t, (ClassName className, x))) . Set.fromList . map fst $ survivors

unifyPending :: Infer ()
unifyPending = getPendingUnifications >>= loop
    where loop pu =
              do  newEntries <- forM (Set.toList pu) unifyAmbiguousEntry
                  let pu' = Set.fromList $ catMaybes newEntries
                  setPendingUnifications pu'
                  when (pu' /= pu) $ loop pu'

--             do  newEntries <- forM (Set.toList pu) $ \entry@((src, ts), t) ->
--                                 do  t' <- applyMainSubst t
--                                     let unifAction = do inst <- instantiate ts >>= assertNoPred
--                                                         inst' <- applyMainSubst inst
--                                                         unify src inst' t'
--                                     result <- runSubInfer $ unifAction >> getState
