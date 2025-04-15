module Heco.Data.Entity.TH where

import Heco.Data.Aeson (defaultAesonOps)
import Heco.Data.Entity (Entity)

import Language.Haskell.TH (Q, Dec (InstanceD), Name, Type(AppT, ConT))
import Data.Aeson.TH (deriveJSON)

deriveEntity :: Name -> Q [Dec]
deriveEntity name = do
    jsonDec <- deriveJSON defaultAesonOps name
    pure $ jsonDec
        ++ [ InstanceD Nothing [] (AppT (ConT ''Entity) (ConT name)) [] ]