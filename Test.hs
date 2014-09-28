{-# LANGUAGE TemplateHaskell #-}
module Test where

import Data.DeriveTH
import Test.QuickCheck

import qualified Data.Map.Lazy as Map
import Data.Functor((<$>))

import Types


$(derive makeArbitrary ''JSConsType)

instance (Arbitrary a) => Arbitrary (Type a) where
    arbitrary = do
      isVar <- arbitrary
      case isVar of
        True -> TVar <$> arbitrary
        False -> do consName <- arbitrary
                    targs <- arbitrary
                    return $ TCons consName targs

    shrink (TVar _) = []
    shrink (TCons name args) = args
                  

instance (Ord k, Arbitrary k, Arbitrary v) => Arbitrary (Map.Map k v) where
    arbitrary = Map.fromList <$> arbitrary
    shrink m = map (flip Map.delete m) (Map.keys m)
