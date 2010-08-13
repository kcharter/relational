{-| Generators for attribute names. -}

module AttrNameGen where

import Control.Monad (liftM)
import Test.QuickCheck

import Relational.Naive.AttrName

instance Arbitrary AttrName where
    arbitrary = attrNames alphas 8
                where alphas = map (:[]) (['a'..'z'] ++ ['A'..'Z'] )
    shrink = const []

-- | Generates attribute names from a non-empty collection of base
-- names, and numeric subscripts ranging from zero to some maximum.
attrNames :: [String] -> Int -> Gen AttrName
attrNames baseNames maxSubscript =
    do baseName <- genBaseName
       (toAttrName . finishName baseName) `liftM` genMaybeSubscript
    where genBaseName = elements baseNames
          genMaybeSubscript =
              frequency [(10, return Nothing),
                         (90, Just `liftM` choose (0, maxSubscript))]
          finishName baseName = maybe baseName ((baseName ++) . show)