{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Heco.Data.Default where

import Data.Default (Default(def))
import Data.Text (Text)

instance Default Text where
    def = ""