{-# OPTIONS_GHC -Wno-orphans #-}

module Heco.Data.Unique where

import Data.Unique (Unique, hashUnique)

instance Show Unique where
    show u = show $ hashUnique u