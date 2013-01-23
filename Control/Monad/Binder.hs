{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, DefaultSignatures, UndecidableInstances #-}
module Control.Monad.Binder where

import Control.Applicative
import Control.Monad.Namer

-- | It's important to tie fresh indexes with contexts.
--   We achive extra flexibility and control when we can freely check
--   if fresh index occurs in current context.
class (Functor m, Applicative m, Monad m) => MonadBinder n t m | m -> n, m -> t where
  lookupVar :: n -> m (Maybe t)

  -- | 'bingVar' are the only place where we can modify context.
  bindVar :: n -> t -> m ()

  newVar  :: t -> m n

  default newVar :: MonadNamer n m => t -> m n
  newVar term = do
    name <- freshen
    bindVar name term
    return name

-- TODO: bindAlias
-- TODO: Instances for ListT, ReaderT, e.t.c
--instance (Error e, MonadBinder n t m) => MonadBinder n t (ErrorT e m)
--instance MonadBinder n t m => MonadBinder n t (ReaderT r m)