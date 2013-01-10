{-# LANGUAGE MultiParamTypeClasses #-}
module Data.Term.Name 
       ( Name(..)
       , NameGen, suffEnumGen
       ) where

import Text.PrettyPrint.ANSI.Leijen

import Data.String
import Data.Set as S

import Data.Term
import Data.Substitution


newtype Name = Name { getName :: String }
               deriving (Show, Read, Eq, Ord)


instance Pretty Name where
  pretty x = char '`' <> text (getName x) <> char '\''

instance IsString Name where
  fromString = Name

--instance Renameable Name Name where
--  freeVars

instance Term Name Name where
  var        = id
  freeVars   = S.singleton
  rename n s = applyWithDef s n n
  subst      = rename
  view       = Id
  

type NameGen s = s -> (Name, s)

suffEnumGen :: (Show i, Enum i) => NameGen i
suffEnumGen i = (Name $ 'a' : show i, succ i)