{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module Data.Substitution
       ( Subst(..), Entry(..)
       ) where

import Data.Maybe
import Data.Mapping.Partial
import Text.PrettyPrint.ANSI.Leijen


data Entry n t = n :=  t
               | n :=? !()
                 deriving (Show, Read, Eq)

instance (Pretty n, Pretty t) => Pretty (Entry n t) where
  pretty (n := t  ) = pretty n <+> text ":=" <+> pretty t
  pretty (n :=? ()) = pretty n <+> text ":=?"


newtype Subst n t = Subst {
    bindings :: [Entry n t]
  } deriving (Show, Read, Eq)


instance (Pretty n, Pretty t) => Pretty (Subst n t) where
  pretty = vcat . map pretty . bindings

instance Eq n => PartialMap n (Subst n) where
  sempty = Subst []

  apply (Subst cxt) name = lookup name $ mapMaybe toPair cxt
    where toPair (a := b) = Just (a, b)
          toPair _ = Nothing

  bind n t (Subst xs) = Subst $ n := t   : xs
  unbind n (Subst xs) = Subst $ n :=? () : xs
