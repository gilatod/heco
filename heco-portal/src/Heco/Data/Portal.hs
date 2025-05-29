module Heco.Data.Portal where

import Heco.Data.Immanant.Terminal (TerminalId)
import Heco.Data.TimePhase (TimePhase)

import Data.Conduit (ConduitT)
import Data.Text (Text)
import Effectful.Dispatch.Dynamic (HasCallStack)

data PortalSignal
    = PortalReply TimePhase Text
    | PortalTick
    | PortalClose

type PortalSignalSource m = ConduitT () PortalSignal m ()
type PortalProcedure m = HasCallStack => TerminalId -> PortalSignalSource m -> m ()
data Portal m = Portal Text (PortalProcedure m)