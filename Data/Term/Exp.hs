{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module Data.Term.Exp where

import Text.PrettyPrint.ANSI.Leijen
import Data.String
import Data.Char

import Data.Foldable
import Data.Traversable

import Data.Term
import Data.Term.Name
import Data.HasNames

data Tm a = Var a
          | Lit Name
          | Tm a :$ Tm a
            deriving (Show, Read, Eq, Ord, Functor, Foldable, Traversable)

infixl 3 :$

instance Pretty a => Pretty (Tm a) where
  pretty (Var a) = pretty a
  pretty (Lit a) = pretty a
  pretty (p :$ q) = parens (pretty p <+> pretty q)

instance Monad Tm where
  return = Var
  Var a  >>= f = f a
  Lit a  >>= _ = Lit a
  p :$ q >>= f = (p >>= f) :$ (q >>= f)

instance HasNames Name (Tm Name)

instance Term Name (Tm Name) where
  view (Var a)  = Id a
  view (Lit a) = Id a
  view (p :$ q) = p :*: q

type Exp = Tm Name

instance IsString s => IsString (Tm s) where
  fromString xs@(x : _)
    | isLower x = Var $ fromString xs
  fromString xs = Lit $ fromString xs

instance Num (Tm Name) where
  fromInteger = fromString . show
