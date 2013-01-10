{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
module Data.Mapping.Partial 
       ( PartialMap (..)
       , applyWithDef
       ) where

import qualified Data.Map as M
import Data.Map (Map)
import Data.Maybe


class PartialMap n s | s -> n where
  sempty :: s t

  apply  :: s t -> n -> Maybe t
  
  -- | If /a/ have been bounded a new binding should override an old one.
  bind   :: n -> t -> s t -> s t
  unbind :: n -> s t -> s t

applyWithDef :: PartialMap n s => s t -> n -> t -> t
applyWithDef s n t = fromMaybe t $ apply s n


instance Ord n => PartialMap n (Map n) where
  sempty = M.empty
  apply = flip M.lookup
  bind   = M.insert
  unbind = M.delete
