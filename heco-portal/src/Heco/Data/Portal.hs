module Heco.Data.Portal where

import Heco.Data.Immanant.Terminal (TerminalId)
import Heco.Data.TimePhase (TimePhase)

import Data.Conduit (ConduitT)
import Data.Text (Text)

data PortalSignal
    = PortalReply TimePhase Text
    | PortalTick
    | PortalClose

type PortalSignalSource m = ConduitT () PortalSignal m ()

data Portal m = Portal
    { name :: Text
    , procedure :: TerminalId -> PortalSignalSource m -> m () }