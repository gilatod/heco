module Heco.Effectful.Agent where

import Heco.Data.TimePhase (TimePhase)

import Effectful (Effect, Eff, (:>))
import Effectful.TH (makeEffect)

data Agent :: Effect where
    WithAgentInteractionEx :: Maybe TimePhase -> m a -> Agent m a

makeEffect ''Agent

withAgentInteraction :: Agent :> es => Eff es a -> Eff es a
withAgentInteraction = withAgentInteractionEx Nothing