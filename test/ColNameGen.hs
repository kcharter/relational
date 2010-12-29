{-| Generators for attribute names. -}

module ColNameGen where

import Test.QuickCheck

import Relational.ColName

import IdentGen

instance Arbitrary ColName where
    arbitrary = tinyIdentifiers
    shrink = const []
