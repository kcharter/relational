-- | A special-purpose collection of bindings from relational variable
-- names to relations. This module is meant to be imported qualified
-- because of name clashes with the prelude and other collection
-- modules.

module Relational.Naive.RelVars (RelVars, empty, size, relNames, toList, fromList, get, contains, put, modify) where

import qualified Data.Map as DM

import Relational.RelName
import Relational.Naive.Relation

newtype RelVars d = RelVars { byName :: DM.Map RelName (Relation d) } deriving (Eq, Ord, Show)

-- | The empty collection of relational variables.
empty :: RelVars d
empty = RelVars { byName = DM.empty }

-- | The number of bindings in a collection of relational variables.
size :: RelVars d -> Int
size = DM.size . byName

-- | The list of relational variable names in this set of bindings.
relNames :: RelVars d -> [RelName]
relNames = DM.keys . byName

-- | Extracts a list of bindings as name-relation pairs.
toList :: RelVars d -> [(RelName, Relation d)]
toList = DM.toList . byName

-- | Builds a set of bindings from a list of name-relation pairs.
fromList :: [(RelName, Relation d)] -> RelVars d
fromList = RelVars . DM.fromList

-- | The binding for a relational variable name, if one is
-- defined.
get :: RelName -> RelVars d -> Maybe (Relation d)
get n = DM.lookup n . byName

-- | Indicates whether a relational variable name is bound by
-- a collection of bindings.
contains :: RelName -> RelVars d -> Bool
contains n = DM.member n . byName

-- | Insert a binding for a relational variable name, possibly
-- deleting an old binding in the process.
put :: RelName -> Relation d -> RelVars d -> RelVars d
put n r vars = RelVars { byName = DM.insert n r (byName vars) }

-- | Modify an existing binding by applying a function on
-- relations. The result is 'Nothing' if and only if there is no existing
-- binding.
modify :: RelName -> (Relation d -> Relation d) ->  RelVars d -> Maybe (RelVars d)
modify n f vars = fmap ((\r -> put n r vars) . f) $ get n vars

