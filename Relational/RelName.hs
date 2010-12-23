{-# LANGUAGE TypeSynonymInstances #-}

module Relational.RelName (RelName, ToRelName(..)) where

import Data.String (IsString(..))
import qualified Data.Text as T

-- Relation names.
newtype RelName = RelName T.Text deriving (Eq, Ord)

instance Show RelName where
  show (RelName t) = "relation " ++ show t
  
-- A class for types that can be used to construct relation names.
class ToRelName a where
  -- Convert to a relation name.
  relName :: a -> RelName
  
instance ToRelName T.Text where
  relName = RelName
  
instance ToRelName String where
  relName = RelName . T.pack
  
instance IsString RelName where
  fromString = RelName . T.pack
  