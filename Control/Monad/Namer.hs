{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances, DefaultSignatures #-}
module Control.Monad.Namer 
       ( MonadNamer ()
       ) where

import Control.Applicative

import Control.Monad.List
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer

import Data.Foldable
import Data.Traversable
import Data.Map as M
import Data.Monoid ()

-- | State monad.
type NameGen i n = i -> (n, i)

-- | Monad which can generate new names and rename terms.
--   When implementing contexts it may be useful 
--   to get unobfuscated context-dependent names.
class (Functor m, Applicative m, Monad m) => MonadNamer n t m | m -> n, m -> t where
  -- | Gives a new unique name in a current context.
  freshen :: m n

  -- freshMap :: f a -> m (Map a n)
  
  -- TODO: freshMapping :: Mapping a m => f a -> m (Map a n)
  -- | Gives a map from 
  freshMap :: (Foldable f, Ord a) => f a -> m (Map a n)
  freshMap = foldrM bindFresh M.empty
    where bindFresh n m = (\n' -> insert n n' m) <$> freshen

  -- | Map all unbounded variable names to a fresh name set. 
  --   Fresh name set should not intersect with previous name sets.
  fresh :: t -> m t

  freshies ::  => t -> m t
  freshies t = freshMap t

-- funny instance:
--          
-- newtype TickNamer a = TN (IO a) deriving (Functor, Applicative, Monad)
--
-- instance MonadNamer Int TickNamer where  
--   freshen = TN getCurrentTick
--

instance (Error e, MonadNamer n t m) => MonadNamer n t (ErrorT e m) where
  freshen = lift freshen
  fresh   = lift . fresh
  freshies = undefined
  
instance MonadNamer n t m => MonadNamer n t (ReaderT r m) where
  freshen = lift freshen
  fresh   = lift . fresh  
  
instance MonadNamer n t m => MonadNamer n t (StateT s m) where
  freshen = lift freshen
  fresh   = lift . fresh  
  
instance (Monoid w, MonadNamer n t m) => MonadNamer n t (WriterT w m) where
  freshen = lift freshen
  fresh   = lift . fresh  
  
instance MonadNamer n t m => MonadNamer n t (ListT m) where
  freshen = lift freshen
  fresh   = lift . fresh