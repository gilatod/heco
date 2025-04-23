module Heco.Data.Entity.TH where

import Heco.Data.Aeson (HasAesonOps, defaultAesonOps)
import Heco.Data.Entity (Entity)

import Language.Haskell.TH
    ( Q,
      Type(ConT, AppT),
      Dec(InstanceD),
      Name )

import Data.Aeson.TH (deriveJSON)

deriveEntity :: Name -> Q [Dec]
deriveEntity name = do
    jsonDecs <- deriveJSON defaultAesonOps name
    pure $ jsonDecs ++
        [ InstanceD Nothing [] (AppT (ConT ''HasAesonOps) nameT) []
        , InstanceD Nothing [] (AppT (ConT ''Entity) nameT) [] ]
    where
        nameT = ConT name