module Heco.Effectful.InternalTimeStream where

import Heco.Data.TimePhase (TimePhase, AnyImmanantContent (AnyImmanantContent), ImmanantContent)

import Effectful (Effect, Eff, (:>))
import Effectful.TH (makeEffect)
import Effectful.Dispatch.Dynamic (HasCallStack)

import Data.Vector (Vector)
import Data.Vector qualified as V

import Control.Monad (void)

data InternalTimeStream :: Effect where
    ProgressPresent :: InternalTimeStream m TimePhase
    Present :: Vector AnyImmanantContent -> InternalTimeStream m TimePhase
    GetPresent :: InternalTimeStream m TimePhase
    GetRetention :: InternalTimeStream m (Vector TimePhase)
    GetRetentionLength :: InternalTimeStream m Int
    GetRetentionCapacity :: InternalTimeStream m Int
    ClearRetention :: InternalTimeStream m ()

makeEffect ''InternalTimeStream

progressPresent_ ::
    (HasCallStack, InternalTimeStream :> es) => Eff es ()
progressPresent_ = void progressPresent

present_ ::
    InternalTimeStream :> es
    => Vector AnyImmanantContent -> Eff es ()
present_ contents = void $ present contents

presentList ::
    InternalTimeStream :> es
    => [AnyImmanantContent] -> Eff es TimePhase
presentList contents = present $ V.fromList contents

presentList_ ::
    InternalTimeStream :> es
    => [AnyImmanantContent] -> Eff es ()
presentList_ contents = void $ presentList contents

presentOne ::
    (InternalTimeStream :> es, ImmanantContent c)
    => c -> Eff es TimePhase
presentOne content = present $ V.singleton $ AnyImmanantContent content

presentOne_ ::
    (InternalTimeStream :> es, ImmanantContent c)
    => c -> Eff es ()
presentOne_ content = void $ presentOne content

getLatestRetent ::
    (HasCallStack, InternalTimeStream :> es)
    => Eff es (Maybe TimePhase)
getLatestRetent = do
    retention <- getRetention
    if V.length retention == 0
        then pure Nothing
        else pure $ Just $ retention V.! (V.length retention - 1)