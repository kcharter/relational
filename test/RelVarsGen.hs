module RelVarsGen where

import Control.Monad (liftM)
import Test.QuickCheck

import qualified Relational.Naive.RelVars as RV

import RelationGen ()
import RelNameGen ()

instance (Ord d, Arbitrary d) => Arbitrary (RV.RelVars d) where
  arbitrary = RV.fromList `liftM` arbitrary
  shrink = map RV.fromList . (:[]) . RV.toList
  
