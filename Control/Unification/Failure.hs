{-# LANGUAGE FlexibleContexts #-}
module Control.Unification.Failure where

import Control.Monad.Error

import Text.PrettyPrint.ANSI.Leijen

data UFailure n t = NoMsgE
                  | StrMsgE String
                  | OccChkE n t
                  | MismatchE n t
                    deriving (Show, Read, Eq)

-- TODO: class UnificationFailure  
instance (Pretty n, Pretty t) => Pretty (UFailure n t) where
  pretty     NoMsgE      = text   "Unknown error!"
  pretty (StrMsgE msg)   = text $ "Error: " ++ msg
  pretty (OccChkE n t)   = text "Symbol" <$$>
                           pretty n      <$$>
                           text "occurs in" <$$> 
                           pretty t <> text "."
                           
  pretty (MismatchE n t) = text "Symbol:" <$> 
                           pretty n      <$>
                           text "do not match with" <$>
                           pretty t <> text "."
  
instance (Pretty p, Pretty q) => Pretty (Either p q) where
  pretty (Left p) = pretty p
  pretty (Right q) = pretty q

instance Error (UFailure n t) where
  noMsg  = NoMsgE
  strMsg = StrMsgE
  
occurCheckFail :: MonadError (UFailure n t) m => n -> t -> m ()
occurCheckFail n t = throwError $ OccChkE n t

matchFail :: MonadError (UFailure n t) m => n -> t -> m ()
matchFail n t = throwError $ MismatchE n t
