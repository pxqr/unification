{-# LANGUAGE FlexibleContexts, FlexibleInstances, ViewPatterns #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, UndecidableInstances #-}
module Control.Unification 
       ( module Data.Term
       -- ^ Basic operations
       , fresh, freshies, unify, reify, reifies
       -- ^ Derived operations
       , (=^=), unifyR
       ) where

import Control.Applicative
import Control.Monad.Error
import Control.Monad.Binder
import Control.Monad.Namer
import Control.Monad.Context
import Control.Unification.Failure
import Data.Monoid
import Data.Term
                        

unify :: ( MonadContext n t m
         , MonadError (UFailure n t) m
         , Term n t) 
         => t -> t -> m ()

-- TODO: trace
-- TODO: View t a = Id a | Ts [t a]
-- unify (view -> Ts ps) (view -> Ts qs) 
--   | length ps /= length qs = mismatch
--   | otherwise = go ps qs
--     where go (x : xs) (y : ys) = do 
--         unify x y 
--         xs' <- reify xs 
--         ys' <- reify ys
--         go xs' ys'
--            go [] [] = return ()
--
unify v@(view -> Id n) a
  |   v == a  = return ()
  | isVar v   = f
  | otherwise = g
  where f |      n `occurIn` a      = occurCheckFail n a
          |        otherwise        = bindVar n a
        
        g |         isVar a         = unify a v --case view a of Id n' -> bindVar n' v
          |        otherwise        = matchFail n a
                           
unify (view -> t1 :*: t2) (view -> t3 :*: t4) = do 
  unify t1 t3
  t2' <- reify t2
  t4' <- reify t4
  unify t2' t4'
  -- or just `solve $ (t1 :~: t3) :+: (t3 :~: t4)'

unify a v = unify v a
-- Termination are guaranteed by View constructors

---------------------------- combinators ---------------------------------------
p =^= q = do
  p' <- fresh p
  q' <- fresh q
  unify p' q'
  reify p'

instance (MonadContext n t m, MonadError (UFailure n t) m, Term n t) => Monoid (m t) where
  mempty = var <$> freshen 
  mp `mappend` mq = do 
    p <- mp
    q <- mq
    p =^= q
  

unifyR :: (MonadContext n t m, MonadError (UFailure n t) m, Term n t) 
          => t -> t -> m t
unifyR p q = unify p q >> reify p

tryUnify :: (MonadContext n t m, MonadError (UFailure n t) m, Term n t) 
          => t -> t -> m (Maybe t)
tryUnify p q = (Just <$> unifyR p q) `catchError` (const $ return Nothing)

----------------------------- Derived ------------------------------------------
  
{-




-}

--class Unifiable t u | u -> t where
--  unify' :: MonadBinder n t m => t -> m ()
{-
instance Term t => Unifiable (t, t) where
data Constraint t n = t n :~: t n
newtype Contstraints t n = Constraints [Constraint t n]
instance Unifiable Constraint 
instance Unifiable [Constraint] 
-}
{-

-- since unification is associative something like
--  Constrint u :<>: Constraint
-- is usefulness

class Unifiable u t | u -> t where
  resolve :: (MonadContext n t m, MonadError (UFailure n t) m, Term n t) => u -> m ()

instance Unifiable (Constraint t) t where
  resolve (p :~: q) = unify p q

instance Unifiable [Constraint t] t where
  resolve [] = return ()
  resolve (x : xs) = resolve x >> reifies xs >>= resolve
-}
{-
class MonadSolver c m where
  resolve :: c -> m ()

instance (MonadContext n t m, MonadError (UFailure n t) m, Term n t) 
         => MonadSolver (t, t) m where
  resolve = uncurry unify 
  
instance MonadSolver (t, t) m => MonadSolver (Constraint t) m where
  resolve (p :~: q) = resolve (p, q)
  resolve (p :+: q) = resolve p >> reify q >>= resolve q
-}
