{-| Generators for attribute names. -}

module ColNameGen where

import Control.Monad (liftM)
import Test.QuickCheck

import Relational.ColName

instance Arbitrary ColName where
    arbitrary = colNames alphas 8
                where alphas = map (:[]) (['a'..'z'] ++ ['A'..'Z'] )
    shrink = const []

-- | Generates column names from a non-empty collection of base
-- names, and numeric subscripts ranging from zero to some maximum.
colNames :: [String] -> Int -> Gen ColName
colNames baseNames maxSubscript =
    do baseName <- genBaseName
       (colName . finishName baseName) `liftM` genMaybeSubscript
    where genBaseName = elements baseNames
          genMaybeSubscript =
              frequency [(10, return Nothing),
                         (90, Just `liftM` choose (0, maxSubscript))]
          finishName baseName = maybe baseName ((baseName ++) . show)