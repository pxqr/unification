{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
module Control.Monad.Context where

import Prelude hiding (mapM)

import Control.Monad.Namer
import Control.Monad.Binder
import Data.Traversable


-- | Failable context.
class (MonadNamer n m, MonadBinder n t m) => MonadContext n t m | m -> t, m -> n where
  -- | Makes something with names concrete in current context.
  reify :: t -> m t

  reifies :: Traversable f => f t -> m (f t)
  reifies = mapM reify

{-
  -- | If computation failed
  annotate  ::  t  -> m a -> m a

  annotates :: [t] -> m a -> m a
  annotates = foldr annotate

data Failure t e = NoMsgE
                 | StrMsgE
                 | Fail [a] e

search :: MonadContext n t m => [m a] -> m [a]
search = runListT . msum . lift
-}