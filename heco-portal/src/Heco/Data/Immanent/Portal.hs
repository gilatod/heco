module Heco.Data.Immanent.Portal where

import Heco.Data.Noema (Noema (Noema))
import Heco.Data.TimePhase (TimePhase)
import Heco.Data.Task (TaskId)
import Heco.Effectful.TaskService (TaskService)
import Heco.Effectful.Intentionality (Intentionality, newObject)

import Effectful (Eff, (:>))
import Effectful.Concurrent.Chan (Chan, Concurrent, newChan)

import Data.Text (Text)

data Portal signal m = Portal
    { noema :: Noema
    , signalChan :: Chan signal
    , procedure :: Chan signal-> m () }

newPortal ::
    ( Concurrent :> es
    , Intentionality :> es
    , TaskService :> es )
    => Text
    -> (Chan signal -> Eff es ())
    -> Eff es (Portal signal (Eff es))
newPortal name procedure = do
    chan <- newChan
    noema <- Noema <$> newObject <*> pure name
    pure Portal
        { noema = noema
        , signalChan = chan
        , procedure = procedure }