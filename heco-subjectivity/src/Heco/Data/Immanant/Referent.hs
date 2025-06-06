module Heco.Data.Immanant.Referent where

import Heco.Data.TimePhase (ImmanantContent(..), TimePhase, getImmanantContent)
import Heco.Effectful.InternalTimeStream (InternalTimeStream, getPresent)

import Effectful ((:>), Eff)

newtype Referent
    = ReferentTimePhase TimePhase
    deriving (Show)

instance ImmanantContent Referent

getReferentTimePhase ::
    ( InternalTimeStream :> es )
    => Eff es (Maybe TimePhase)
getReferentTimePhase = do
    present <- getPresent
    case getImmanantContent @Referent present of
        Nothing -> pure Nothing
        Just (ReferentTimePhase timePhase) ->
            pure $ Just timePhase