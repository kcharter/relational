{-# LANGUAGE TypeSynonymInstances #-}

module Relational.RelVar (RelVar, ToRelVar(..)) where

import Data.String (IsString(..))
import qualified Data.Text as T

-- Relational variable names.
newtype RelVar = RelVar T.Text deriving (Eq, Ord)

instance Show RelVar where
  show (RelVar t) = "relvar " ++ show t
  
-- A class for types that can be used to construct relational variable
-- names.
class ToRelVar a where
  -- Convert to a relational variable name.
  relVar :: a -> RelVar
  
instance ToRelVar T.Text where
  relVar = RelVar
  
instance ToRelVar String where
  relVar = RelVar . T.pack
  
instance IsString RelVar where
  fromString = RelVar . T.pack
  