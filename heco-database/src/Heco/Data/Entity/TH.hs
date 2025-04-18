module Heco.Data.Entity.TH where

import Heco.Data.Aeson (AesonDefault(..))
import Heco.Data.Entity (Entity)

import Language.Haskell.TH
    ( Q,
      Type(ConT, AppT),
      Dec(InstanceD, StandaloneDerivD),
      Name,
      DerivStrategy(ViaStrategy) )
import Data.Aeson (ToJSON, FromJSON)

deriveEntity :: Name -> Q [Dec]
deriveEntity name = pure
    [ StandaloneDerivD (Just (ViaStrategy (AppT (ConT ''AesonDefault) nameT))) []
        (AppT (ConT ''ToJSON) nameT)
    , StandaloneDerivD (Just (ViaStrategy (AppT (ConT ''AesonDefault) nameT))) []
        (AppT (ConT ''FromJSON) nameT)
    , InstanceD Nothing [] (AppT (ConT ''Entity) nameT) [] ]
    where
        nameT = ConT name