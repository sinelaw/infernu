module Infernu.Pred
       (toDNF, toList)
    where 


import Infernu.Types (TPred(..))

data CanonPredOr t = CanonPredOr [CanonPredAnd t]
                     deriving (Show, Eq)

data CanonPredAnd t = CanonPredAnd [CanonPredEq t]
                      deriving (Show, Eq)

data CanonPredEq t = CanonPredEq TVarName t

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
-- >>> toList $ TPredAnd (TPredOr (TPredEq 0 'a') (TPredEq 1 'b')) (TPredOr (TPredEq 0 'c') (TPredEq 1 'd'))
-- [[TPredAnd (TPredEq 0 'a') (TPredEq 0 'c')],[TPredAnd (TPredEq 0 'a') (TPredEq 1 'd')],[TPredAnd (TPredEq 1 'b') (TPredEq 0 'c')],[TPredAnd (TPredEq 1 'b') (TPredEq 1 'd')]]
toList :: Eq t => TPred t -> [[TPred t]]                                      
toList = toList' . toDNF
    where toList' p@(TPredAnd _ _) = [[p]]
          toList' (TPredOr p1 p2) = toList' p1 ++ toList' p2
          toList' p = [[TPredAnd p TPredNothing]]
          


fromList :: [[TPred t]] -> TPred t
fromList [] = TPredNothing
fromList [[]] = TPredNothing
fromList [x:xs] = TPredOr x (fromListAnd xs)
    where fromListAnd [] = TPredNothing
          fromListAnd (x:xs) = TPredAnd x (fromListAnd xs)
    
unifyPreds :: Eq t => (t -> t -> Bool) -> TPred t -> TPred t -> Maybe (Pred t)
unifyPreds uni p1 p2 = unifyPreds' (toList p1) (toList p2)
    where unifyPreds' [] p2' = Right $ fromList p2'
          unifyPreds' p1' [] = Right $ fromList p1'
          unifyPreds' 
