{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Control.Unification.Context where

import Prelude hiding (foldr)

import Control.Applicative
import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Error
import Control.Monad.Namer
import Control.Monad.Binder
import Control.Monad.Context

import Data.Foldable
import Data.Traversable
import qualified Data.Map as M
import qualified Data.Set as S

import Data.Term
import Data.HasNames
import Data.Substitution
import Data.Mapping.Partial


newtype ContextT i n t m a = ContextT {
    unContext :: ReaderT (NameGen i n) (StateT (i, Subst n t) m) a
  } deriving (Functor, Applicative, Alternative, Monad, MonadError e)

-- forall i. ?
-- Extra safiety: `forall i' guarantees that ix never escape.
-- So all that we can do with `i' is apply to name generator to get fresh name.
-- unfortunately, neither GeneralizedNewtypeDeriving nor StandaloneDeriving
-- works with Rank2Types, so we wont abstract out `i'.


type Context i n t = ContextT i n t Identity


runContextT :: Monad m => ContextT i n t m a -> NameGen i n -> i -> m a
runContextT c g i = evalStateT (runReaderT (unContext c) g) (i, Subst [])

runContext :: Context i n t a -> NameGen i n -> i -> a
runContext c g i = runIdentity $ runContextT c g i


instance Monad m => MonadState (Subst n t) (ContextT i n t m) where
  get = ContextT $ gets snd
  put c = do
    ix <- ContextT $ gets fst
    ContextT $ put $ (ix, c)

instance (Functor m, Applicative m, Monad m, HasNames n t)
         => MonadNamer n (ContextT i n t m) where
  freshen = do
    gen       <- ContextT $ ask
    (ix, cxt) <- ContextT $ get
    let (name, ix') = gen ix
    ContextT $ put (ix', cxt)
    return name

instance (Functor m, Applicative m, Monad m, Term n t)
         => MonadBinder n t (ContextT i n t m) where

  bindVar n t = modify $ bind n t
  lookupVar n = (`apply` n) <$> get

-- TODO compact aliases
instance (Functor m, Applicative m, Monad m, Term n t, Eq t)
         => MonadContext n t (ContextT i n t m) where

  reify t = subst t <$> get >>= \ x -> if x == t then return x else reify x
