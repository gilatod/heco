module Heco.Data.Portal where

import Heco.Data.Immanant.Terminal (TerminalId)

import Data.Conduit (ConduitT)
import Data.Text (Text)

data PortalSignal
    = PortalReply Text
    | PortalClose

type PortalProcedure m = TerminalId -> ConduitT () PortalSignal m () -> m ()

data Portal m = Portal
    { name :: Text
    , procedure :: PortalProcedure m }