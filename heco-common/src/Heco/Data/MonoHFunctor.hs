module Heco.Data.MonoHFunctor where

class MonoHFunctor r where 
    ohmap :: (forall a. m a -> m' a) -> r m -> r m'