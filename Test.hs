{-# LANGUAGE TemplateHaskell, TypeSynonymInstances, FlexibleInstances #-}
module Test where

import Data.DeriveTH
import Test.QuickCheck
import           Test.QuickCheck.All
    
import qualified Data.Map.Lazy as Map
import Data.Functor((<$>))

import Types2

prop_composeSubst' :: TSubst -> TSubst -> Type TBody -> Bool
prop_composeSubst' = prop_composeSubst
    
$(derive makeArbitrary ''TBody)
$(derive makeArbitrary ''Type)

-- instance (Arbitrary a) => Arbitrary (Type a) where
--     arbitrary =
--       oneof [ TVar <$> arbitrary
--             , do consName <- resize 5 arbitrary
--                  targs <- resize 4 arbitrary
--                  return $ TCons consName targs
--             ]

--     shrink (TVar _) = []
--     shrink (TCons name args) = args
                  

instance (Ord k, Arbitrary k, Arbitrary v) => Arbitrary (Map.Map k v) where
    arbitrary = Map.fromList <$> resize 3 arbitrary
    shrink m = map (flip Map.delete m) (Map.keys m)


return []
runAllTests = $(quickCheckAll)
               
