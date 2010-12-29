module IdentGen where

import Control.Monad (liftM)
import Test.QuickCheck

import Data.String (IsString(..))
       
-- | Generates identifiers that start with a single lower-case
-- or upper-case Roman letter, possibly followed by a numeric
-- subscript from 0 to 8.
tinyIdentifiers :: (IsString a) => Gen a
tinyIdentifiers = identifiers alphas 8
  where alphas = map (:[]) (['a'..'z'] ++ ['A'..'Z'])

-- | Generates reasonable random identifiers for an instance of
-- 'IsString'. Each identifier starts with one of a given sequence of
-- base names, usually with a numeric subscript between 0 and a given
-- maximum.
identifiers :: (IsString a) => [String] -> Int -> Gen a
identifiers baseNames maxSubscript  =
    do baseName <- genBaseName
       (fromString . finishName baseName) `liftM` genMaybeSubscript
    where genBaseName = elements baseNames
          genMaybeSubscript =
              frequency [(10, return Nothing),
                         (90, Just `liftM` choose (0, maxSubscript))]
          finishName baseName = maybe baseName ((baseName ++) . show)