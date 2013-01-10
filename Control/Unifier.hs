module Control.Unifier 
       ( Unifier
       , evalUnifier
       , run
       ) where

import Control.Unification.Context
import Control.Unification.Failure

import Data.Term.Name

-- | 'Int' indexed 'Context' with Errors.
type Unifier n t a = ContextT Int n t (Either (UFailure n t)) a
type Result n t = Either (UFailure n t)

evalUnifier :: Unifier n t a -> (Int -> (n, Int)) -> Int -> Result n t a
evalUnifier u g s = runContextT u g s

defSeed :: Int
defSeed = 0

run :: Unifier Name t a -> Result Name t a
run x = evalUnifier x suffEnumGen defSeed
