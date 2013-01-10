{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable, ViewPatterns #-}
{-# LANGUAGE DefaultSignatures, UndecidableInstances #-}
module Data.Term where

import Prelude hiding (foldr, foldr1)

import Data.Foldable
import Data.Traversable
import qualified Data.Set as S
import Data.Set (Set)

import Data.Substitution


class TermDef n t | t -> n where
  varDef      :: n -> t
  freeVarsDef :: t -> Set n
  renameDef   :: Subst n s => t -> s n -> t
  substDef    :: Subst n s => t -> s t -> t

instance (Functor t, Foldable t, Monad t, Ord n) => TermDef n (t n) where
  varDef = return
  freeVarsDef = foldr S.insert S.empty  
  substDef t c = t >>= \n -> maybe (return n) id $ apply c n  
  renameDef t m = fmap (\x -> applyWithDef m x x) t


-- | Views uses to obtain constrants in unification algorithm.
data View t n = Id n
              | t :*: t
                deriving (Show, Read, Eq, Ord, Functor, Foldable, Traversable)


-- | Minimal complete definition 'view' of 'match'.
class (Eq t, Ord n) => Term n t | t -> n where
  -- | Contructs variable from variable name.
  --   By default 'var' = 'return'.
  var :: n -> t
  
  default var :: TermDef n t => n -> t
  var = varDef


  -- | Gives all free variables for a given term.
  --   By default all `holes' of functor are free variables. 
  --   Such behaviour can be overriden.
  freeVars :: t -> Set n
  
  default freeVars :: TermDef n t => t -> Set n
  freeVars = freeVarsDef
  
  -- | Rename variable
  rename :: Subst n s => t -> s n -> t

  default rename :: (TermDef n t, Subst n s) => t -> s n -> t
  rename = renameDef
  
  -- | Test if it's var.
  isVar :: t -> Bool
  isVar v@(view -> Id n) = n `occurIn` v
  isVar _                = False


  -- | Test occurence of variable name in term.
  occurIn :: n -> t -> Bool
  occurIn n v = n `S.member` freeVars v


  -- | Since term can have constants in holes it's necessary to keep names 
  --   /closed/ in respect of substitutions. In either case it's more convenient
  --   to explicitly map names.
  subst :: Subst n s => t -> s t -> t
  
  default subst :: (TermDef n t, Subst n s) => t -> s t -> t
  subst = substDef


  view :: t -> View t n
--  match :: t n -> t n -> Constraint

instance Term n t => Term n [t] where
  var x    = [var x]
  freeVars = fold . fmap freeVars
  rename ts s = fmap (`rename` s) ts
  subst ts s = undefined -- fmap (`subst` s) ts

(|$|) :: (Term n t, Subst n s, Ord n) => s t -> t -> t
(|$|) = flip subst
{-# INLINE (|$|) #-}
