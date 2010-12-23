{-# LANGUAGE TypeSynonymInstances #-}

-- | Column names for relations.

module Relational.ColName (ColName, ToColName(..)) where

import Data.String (IsString(..))
import qualified Data.Text as T

newtype ColName = ColName T.Text deriving (Eq, Ord, Show)

-- | Types that can be converted into column names.
class ToColName a where
  colName :: a -> ColName

instance ToColName String where
  colName = ColName . T.pack
  
instance ToColName T.Text where
  colName = ColName
  
instance ToColName ColName where
  colName = id
  
instance IsString ColName where
  fromString = ColName . T.pack
  

  