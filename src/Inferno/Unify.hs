{-# LANGUAGE CPP             #-}
{-# LANGUAGE TupleSections   #-}
{-# LANGUAGE BangPatterns    #-}

module Inferno.Unify
       (unify, unifyAll, unifyl)
       where


import           Data.Monoid                (Monoid(..))
import           Control.Monad              (foldM)
import           Data.Traversable              (Traversable (..))
import           Data.Functor               ((<$>))

import qualified Data.Map.Lazy              as Map
import           Data.Map.Lazy              (Map)
import           Data.Maybe                 (mapMaybe)
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


import           Inferno.BuiltinArray ( arrayRowType)
import           Inferno.Pretty
import           Inferno.Types

import           Inferno.Decycle
import           Inferno.InferState
import           Inferno.Log

----------------------------------------------------------------------
tryMakeRow :: FType Type -> Maybe (TRowList Type)
tryMakeRow (TCons TArray [t]) = Just $ arrayRowType t
tryMakeRow _ = Nothing

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
-- >>> let tvar3 = Fix $ TBody $ TVar 3
-- >>> let recRow = Fix $ TRow $ TRowProp "x" tvar0 $ TRowProp "y" tvar3 (TRowEnd $ Just $ RowTVar 2)
-- >>> let s = fromRight $ u tvar0 recRow
-- >>> s
-- fromList [(0,Fix (TCons (TName (TypeId 1)) [Fix (TBody (TVar 2)),Fix (TBody (TVar 3))]))]
-- >>> applySubst s tvar0
-- Fix (TCons (TName (TypeId 1)) [Fix (TBody (TVar 2)),Fix (TBody (TVar 3))])
--
-- >>> :{
-- pretty $ runInfer $ do
--     s <- unify p tvar0 recRow
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
--     let tvar4 = Fix . TBody . TVar $ 4
--         tvar5 = Fix . TBody . TVar $ 5
--     s2 <- unify p (applySubst s1 recRow) (applySubst s1 $ Fix $ TRow $ TRowProp "x" tvar4 $ TRowProp "y" tvar5 (TRowEnd Nothing))
--     return $ applySubst (s2 `composeSubst` s1) recRow
-- :}
-- "{x: <Named Type: mu 'B'. {} f>, y: f}"
--
-- >>> let rec2 = Fix $ TCons TFunc [recRow, Fix $ TBody TNumber]
-- >>> :{
-- pretty $ runInfer $ do
--     s1 <- unify p tvar0 rec2
--     return $ applySubst s1 rec2
-- :}
-- "(this: {x: <Named Type: mu 'B'. c d>, y: d, ..c} -> TNumber)"
--
-- >>> :{
-- runInfer $ do
--     s1 <- unify p tvar0 rec2
--     s2 <- unify p tvar0 rec2
--     return $ (applySubst s1 rec2 == applySubst s2 rec2)
-- :}
-- Right True
--
--
-- Test generalization/instantiation of recursive types
--
-- >>> :{
-- pretty $ runInfer $ do
--     s1 <- unify p tvar0 rec2
--     generalize Map.empty $ applySubst s1 rec2
-- :}
-- "forall c d. (this: {x: <Named Type: mu 'B'. c d>, y: d, ..c} -> TNumber)"
--
-- >>> :{
-- putStrLn $ fromRight $ runInfer $ do
--     s1 <- unify p tvar0 rec2
--     tscheme <- generalize Map.empty $ applySubst s1 rec2
--     forM_ [1,2..10] $ const fresh
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
unify :: UnifyF
unify = decycle3 unify''

unify'' :: Maybe UnifyF -> UnifyF
unify'' Nothing _ t1 t2 =
  do traceLog ("breaking infinite recursion cycle, when unifying: " ++ pretty t1 ++ " ~ " ++ pretty t2) ()
     return nullSubst
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
unify' recurse a t1@(TCons (TName n1) targs1) t2 =
  do t1' <- unrollName a n1 targs1
     traceLog ("unrolled: " ++ pretty t1 ++ " ==> " ++ pretty t1' ++ " for unification with ~ " ++ pretty t2) ()
     recurse a t1' (Fix t2) -- unificationError a (unFix t1') t2
unify' recurse a t1 t2@(TCons (TName _) _) = recurse a (Fix t2) (Fix t1)
unify' _ a t1@(TBody _) t2@(TCons _ _) = unificationError a t1 t2
unify' recurse a t1@(TCons _ _) t2@(TBody _) = recurse a (Fix t2) (Fix t1)
unify' recurse a t1@(TCons n1 ts1) t2@(TCons n2 ts2) =
    if (n1 == n2) && (length ts1 == length ts2)
    then fmap (tracePretty ("unified TCons's (" ++ show n1 ++ "): ")) <$> unifyl recurse a nullSubst $ zip ts1 ts2
    else unificationError a t1 t2 --throwError $ "TCons names or number of parameters do not match: " ++ pretty n1 ++ " /= " ++ pretty n2
unify' r a t1@(TRow _)    t2@(TCons _ _) =
  case tryMakeRow t2 of
   Nothing -> unificationError a t1 t2
   Just rowType -> unify' r a t1 (TRow rowType)
unify' r a t1@(TCons _ _) t2@(TRow _)    = unify' r a t2 t1
unify' _ a t1@(TRow _)    t2@(TBody _)   = unificationError a t1 t2
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
              | n `Set.member` freeTypeVars t = let f = minifyVarsFunc t in throwError a $ "Occurs check failed: " ++ pretty (f n) ++ " in " ++ pretty (mapVarNames f t)
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
