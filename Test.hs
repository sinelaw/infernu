{-# LANGUAGE TemplateHaskell, TypeSynonymInstances, FlexibleInstances #-}
module Test where

import Data.DeriveTH
import Test.QuickCheck

import qualified Data.Map.Lazy as Map
import Data.Functor((<$>))

import Types


$(derive makeArbitrary ''JSConsType)

instance (Arbitrary a) => Arbitrary (Type a) where
    arbitrary =
      oneof [ TVar <$> arbitrary
            , do consName <- resize 5 arbitrary
                 targs <- resize 4 arbitrary
                 return $ TCons consName targs
            ]

    shrink (TVar _) = []
    shrink (TCons name args) = args
                  

instance (Ord k, Arbitrary k, Arbitrary v) => Arbitrary (Map.Map k v) where
    arbitrary = Map.fromList <$> resize 10 arbitrary
    shrink m = map (flip Map.delete m) (Map.keys m)

