{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
module Data.HasNames where

import Prelude hiding (foldr)

import Data.Mapping.Partial
import Data.Set as S (Set, union, insert, empty)
import Data.Foldable
import Data.Function

class HasNamesDef n t | t -> n where
  freeVarsDef :: t -> Set n
  renameDef   :: PartialMap n m => t -> m n -> t

instance (Functor t, Foldable t, Ord n) => HasNamesDef n (t n) where
  freeVarsDef = foldr S.insert S.empty
  renameDef t m = fmap (\x -> applyWithDef m x x) t



class Ord n => HasNames n t | t -> n where
  -- | Rename all occurences of free variables.
  rename :: PartialMap n m => t -> m n -> t

  default rename :: (HasNamesDef n t, PartialMap n m) => t -> m n -> t
  rename = renameDef

  -- | Gives all free variables for a given term.
  --   By default all `holes' of functor are free variables.
  --   Such behaviour can be overriden.
  freeVars :: t -> Set n

  default freeVars :: HasNamesDef n t => t -> Set n
  freeVars = freeVarsDef



instance (HasNames n t, Ord n) => HasNames n [t] where
  freeVars = fold . fmap freeVars
  rename t m = fmap (`rename` m) t

instance (HasNames n t, Ord n) => HasNames n (t, t) where
  freeVars = uncurry (union `on` freeVars)
  rename t m = fmap (`rename` m) t
