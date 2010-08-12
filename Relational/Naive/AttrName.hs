{-# LANGUAGE TypeSynonymInstances #-}

module Relational.Naive.AttrName (AttrName,
                                  ToAttrName(..),
                                  FromAttrName(..)) where

import qualified Data.Text as T

newtype AttrName = AttrName T.Text deriving (Eq, Ord)

instance Show AttrName where
    show (AttrName n) = T.unpack n

class ToAttrName a where
    toAttrName :: a -> AttrName

class FromAttrName a where
    fromAttrName :: AttrName -> a

instance ToAttrName AttrName where
    toAttrName = id

instance ToAttrName T.Text where
    toAttrName = AttrName

instance ToAttrName String where
    toAttrName = AttrName . T.pack

instance FromAttrName AttrName where
    fromAttrName = id

instance FromAttrName T.Text where
    fromAttrName (AttrName t) = t

instance FromAttrName String where
    fromAttrName (AttrName t) = T.unpack t



