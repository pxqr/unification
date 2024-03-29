{-# LANGUAGE MultiParamTypeClasses #-}
module Data.Term.Name
       ( Name(..)
       , NameGen, suffEnumGen
       ) where

import Text.PrettyPrint.ANSI.Leijen

import Data.String
import Data.Set as S

import Data.Term
import Data.HasNames
import Data.Mapping.Partial


newtype Name = Name { getName :: String }
               deriving (Show, Read, Eq, Ord)


instance Pretty Name where
  pretty x = text (getName x)

instance IsString Name where
  fromString = Name

instance HasNames Name Name where
  freeVars   = S.singleton
  rename n m = applyWithDef m n n

instance Term Name Name where
  var        = id
  subst      = rename
  view       = Id


type NameGen s = s -> (Name, s)

suffEnumGen :: (Show i, Enum i) => NameGen i
suffEnumGen i = (Name $ 'a' : show i, succ i)