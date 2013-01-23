{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable, ViewPatterns #-}
{-# LANGUAGE DefaultSignatures #-}
module Data.Term
       ( HasNames(..), HasNamesDef(..)
       , Term(..), TermDef(..), View(..)
       , (|$|)
       ) where

import Prelude hiding (foldr, foldr1)

import Data.Foldable
import Data.Traversable
import qualified Data.Set as S
import Data.HasNames

import Data.Mapping.Partial

-- TODO: class HasNames n t | t -> n where freeVars; rename
--

-- | Since t has kind * we doing the following trick in order to make
--   default implementations.
class TermDef n t | t -> n where
  varDef      :: n -> t
  substDef    :: PartialMap n m => t -> m t -> t

instance Monad t => TermDef n (t n) where
  varDef = return
  substDef t c = t >>= \n -> maybe (return n) id $ apply c n


-- | Views uses to obtain constrants in unification algorithm.
data View t n = Id n
              | t :*: t
                deriving (Show, Read, Eq, Ord, Functor, Foldable, Traversable)


-- | Minimal complete definition 'view' or 'match'.
class HasNames n t => Term n t | t -> n where
  -- | Contructs variable from variable name.
  --   By default 'var' = 'return'.
  var :: n -> t

  default var :: TermDef n t => n -> t
  var = varDef

  -- | Test if it's var.
  isVar :: t -> Bool

  default isVar :: Ord n => t -> Bool
  isVar v@(view -> Id n) = n `occurIn` v
  isVar _                = False

  -- | Test occurence of variable name in term.
  occurIn :: n -> t -> Bool

  default occurIn :: (HasNames n t, Ord n) => n -> t -> Bool
  occurIn n v = n `S.member` freeVars v

  -- | Since term can have constants in holes it's necessary to keep names
  --   /closed/ in respect of substitutions. In either case it's more convenient
  --   to explicitly map names.
  subst :: PartialMap n s => t -> s t -> t

  default subst :: (TermDef n t, PartialMap n s) => t -> s t -> t
  subst = substDef


  view :: t -> View t n
--  match :: t n -> t n -> Constraint


(|$|) :: (Term n t, PartialMap n s, Ord n) => s t -> t -> t
(|$|) = flip subst
{-# INLINE (|$|) #-}
