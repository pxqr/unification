{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
module Control.Monad.Context where

import Prelude hiding (mapM)

import Control.Monad.Namer
import Control.Monad.Binder
import Data.Traversable


-- | Failable context.
class (MonadNamer n t m, MonadBinder n t m) => MonadContext n t m | m -> t, m -> n where
  -- | Makes something with names concrete in current context.
  reify :: t -> m t

  reifies :: Traversable f => f t -> m (f t)
  reifies = mapM reify