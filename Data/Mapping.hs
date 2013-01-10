module Data.Mapping where

class Mapping m where
  apply  :: m a b -> a -> b
  bind   :: Eq a => a -> b -> m a b -> m a b


instance Mapping (->) where
  apply = id
  
  bind n t s x
    |  n == x   = t
    | otherwise = s x
