module Heco.Data.Aeson.TH where

import Heco.Data.Aeson (AesonDefault(..), HasAesonOps)

import Language.Haskell.TH
    ( Q,
      Type(ConT, AppT),
      Dec(StandaloneDerivD, InstanceD),
      Name,
      DerivStrategy(ViaStrategy) )
import Data.Aeson (ToJSON, FromJSON)

deriveHasAesonOps :: Name -> Q [Dec]
deriveHasAesonOps name = pure
    [ InstanceD Nothing [] (AppT (ConT ''HasAesonOps) (ConT name)) [] ]

deriveFromJSON :: Name -> Q [Dec]
deriveFromJSON name = pure
    [ StandaloneDerivD (Just (ViaStrategy (AppT (ConT ''AesonDefault) nameT))) []
        (AppT (ConT ''FromJSON) nameT) ]
    where
        nameT = ConT name

deriveFromJSON' :: Name -> Q [Dec]
deriveFromJSON' name = do
    a <- deriveHasAesonOps name
    b <- deriveFromJSON name
    pure $ a ++ b

deriveToJSON :: Name -> Q [Dec]
deriveToJSON name = pure
    [ StandaloneDerivD (Just (ViaStrategy (AppT (ConT ''AesonDefault) nameT))) []
        (AppT (ConT ''ToJSON) nameT) ]
    where
        nameT = ConT name

deriveToJSON' :: Name -> Q [Dec]
deriveToJSON' name = do
    a <- deriveHasAesonOps name
    b <- deriveToJSON name
    pure $ a ++ b

deriveJSON :: Name -> Q [Dec]
deriveJSON name = do
    a <- deriveFromJSON name
    b <- deriveToJSON name
    pure $ a ++ b

deriveJSON' :: Name -> Q [Dec]
deriveJSON' name = do
    a <- deriveHasAesonOps name
    b <- deriveFromJSON name
    c <- deriveToJSON name
    pure $ a ++ b ++ c
