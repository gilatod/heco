module Ae.Sia.SiaQuery where

import Effectful ( Effect )

import Ae.Sia.Types ( QueryPlaceholder, QuerySelectable )

data SiaQuery :: Effect where
    AnyOf :: [SiaQuery m ()] -> SiaQuery m ()
    NoneOf :: [SiaQuery m ()] -> SiaQuery m ()
    Select :: (QuerySelectable s) => SiaQuery m (QueryPlaceholder s)