{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances, DefaultSignatures #-}
module Control.Monad.Namer
       ( MonadNamer (..)
       , NameGen
       , fresh, freshVar
       ) where

import Control.Applicative

import Control.Monad.List
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer

import Data.Foldable
import Data.Map as M
import Data.Monoid ()

import Data.HasNames
import Data.Term

-- | State monad.
type NameGen i n = i -> (n, i)

-- | Monad which can generate new names and rename terms.
--   When implementing contexts it may be useful
--   to get unobfuscated context-dependent names.
class (Functor m, Applicative m, Monad m) => MonadNamer n m | m -> n where
  -- | Gives a new unique name in a current context.
  freshen :: m n

  -- | Gives a map from
  freshMap :: (Foldable f, Ord a) => f a -> m (Map a n)
  freshMap = foldrM bindFresh M.empty
    where
      bindFresh n m = (\n' -> insert n n' m) <$> freshen


-- | Map all unbounded variable names to a fresh name set.
--   Fresh name set should not intersect with previous name sets.
fresh :: (MonadNamer n m, HasNames n t) => t -> m t
fresh t = rename t <$> freshMap (freeVars t)

freshVar :: (MonadNamer n m, Term n t) => m t
freshVar = var <$> freshen

-- funny instance:
--
-- newtype TickNamer a = TN (IO a) deriving (Functor, Applicative, Monad)
--
-- instance MonadNamer Int TickNamer where
--   freshen = TN getCurrentTick
--

instance (Error e, MonadNamer n m) => MonadNamer n (ErrorT e m) where
  freshen = lift freshen

instance MonadNamer n m => MonadNamer n (ReaderT r m) where
  freshen = lift freshen

instance MonadNamer n m => MonadNamer n (StateT s m) where
  freshen = lift freshen

instance (Monoid w, MonadNamer n m) => MonadNamer n (WriterT w m) where
  freshen = lift freshen

instance MonadNamer n m => MonadNamer n (ListT m) where
  freshen = lift freshen
