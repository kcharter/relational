module RelNameGen where

import Test.QuickCheck

import Relational.RelName

import IdentGen

instance Arbitrary RelName where
  arbitrary = identifiers ("R" : map (("R" ++) . (:[])) (['a'..'z'])) 10
  shrink = const []
