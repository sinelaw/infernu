module Infernu.Pred
       (toDNF)
    where 


import Infernu.Types (TPred(..), TVarName)

data CanonPredOr t = CanonPredOr [CanonPredAnd t]
                     deriving (Show, Eq)

data CanonPredAnd t = CanonPredAnd [CanonPredEq t]
                      deriving (Show, Eq)

data CanonPredEq t = CanonPredEq TVarName t
                      deriving (Show, Eq)

-- | Converts a predicate to DNF (Disjunction Normal Form)
-- based on code from the hatt package
--
-- >>> toDNF $ TPredAnd (TPredEq 0 'a') (TPredEq 1 'b')
-- TPredAnd (TPredEq 0 'a') (TPredEq 1 'b')
-- >>> toDNF $ TPredOr (TPredEq 0 'a') (TPredEq 1 'b')
-- TPredOr (TPredEq 0 'a') (TPredEq 1 'b')    
-- >>> let or' = TPredOr (TPredEq 0 'a') (TPredEq 1 'b')
-- >>> toDNF $ TPredAnd or' or'
-- TPredOr (TPredEq 0 'a') (TPredEq 1 'b')
-- >>> toDNF $ TPredAnd (TPredOr (TPredEq 0 'a') (TPredEq 1 'b')) (TPredOr (TPredEq 0 'c') (TPredEq 1 'd'))
-- TPredOr (TPredOr (TPredAnd (TPredEq 0 'a') (TPredEq 0 'c')) (TPredAnd (TPredEq 0 'a') (TPredEq 1 'd'))) (TPredOr (TPredAnd (TPredEq 1 'b') (TPredEq 0 'c')) (TPredAnd (TPredEq 1 'b') (TPredEq 1 'd')))
toDNF :: Eq t => TPred t -> TPred t
toDNF (TPredAnd exp1 exp2) = if exp1 == exp2
                             then toDNF exp1
                             else toDNF exp1 `dist` toDNF exp2
    where
        dist (TPredOr e11 e12) e2 = (e11 `dist` e2) `disj` (e12 `dist` e2)
        dist e1 (TPredOr e21 e22) = (e1 `dist` e21) `disj` (e1 `dist` e22)
        dist e1 e2                = if e1 == e2 then e1 else e1 `TPredAnd` e2
toDNF (TPredOr exp1 exp2) = toDNF exp1 `disj` toDNF exp2
toDNF expr                    = expr

disj :: Eq t => TPred t -> TPred t -> TPred t
disj e1 e2 = if e1 == e2
             then e1
             else e1 `TPredOr` e2

-- | Converts a predicate to a list of sums of products
toCanon :: Eq t => TPred t -> CanonPredOr t
toCanon = CanonPredOr . toCanonOr' . toDNF
    where toCanonOr' (TPredOr p1 p2) = toCanonOr' p1 ++ toCanonOr' p2
          toCanonOr' (TPredAnd p1 p2) = [CanonPredAnd $ (toCanonAnd' p1) ++ (toCanonAnd' p2)]
          toCanonOr' p = [CanonPredAnd $ toCanonAnd' p]

          toCanonAnd' (TPredOr _ _) = error "toDNF didn't supply DNF..."
          toCanonAnd' (TPredAnd p1 p2) = (toCanonAnd' p1) ++ (toCanonAnd' p2)
          toCanonAnd' (TPredEq v t) = [CanonPredEq v t]
          toCanonAnd' TPredNothing = []

fromCanon :: CanonPredOr t -> TPred t
fromCanon (CanonPredOr []) = TPredNothing
fromCanon (CanonPredOr (p:ps)) = foldr TPredOr (fromCanonAnd p) $ map fromCanonAnd ps
    where fromCanonAnd (CanonPredAnd []) = TPredNothing
          fromCanonAnd (CanonPredAnd (p:ps)) = foldr TPredAnd (fromCanonEq p) (map fromCanonEq ps)
          fromCanonEq (CanonPredEq v t) = TPredEq v t
    
-- unifyPreds :: Eq t => (t -> t -> Bool) -> TPred t -> TPred t -> Maybe (Pred t)
-- unifyPreds uni p1 p2 = unifyPreds' (toList p1) (toList p2)
--     where unifyPreds' [] p2' = Right $ fromList p2'
--           unifyPreds' p1' [] = Right $ fromList p1'
--           unifyPreds' 
