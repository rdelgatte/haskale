module Generators where

import           Data.Scientific
import           Model.Beer                     (Beer)
import           Model.BeerStyle                (BeerStyle)
import           Test.QuickCheck
import           Test.QuickCheck.Arbitrary.ADT  (genericArbitrary)
import           Test.QuickCheck.Instances.Text ()

instance Arbitrary Beer where
  arbitrary = genericArbitrary

instance Arbitrary BeerStyle where
  arbitrary = genericArbitrary

instance Arbitrary Scientific where
  arbitrary = scientific <$> choose (0, 15) <*> choose (-2, 0)
