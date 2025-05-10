module Heco.Conduit.Concurrent where

import Effectful (Eff, (:>))
import Effectful.Concurrent (Concurrent)
import Effectful.Concurrent.STM
    ( atomically, newTChanIO, tryReadTChan, writeTChan, atomically )
import Effectful.Concurrent.Async (concurrently)

import Conduit (ConduitT, (.|))
import Conduit qualified as C

mergeSources ::
    (Concurrent :> es, m ~ Eff es)
    => ConduitT () o m () -> ConduitT () o m () -> ConduitT () o m ()
mergeSources src1 src2 = do
    chan <- C.lift newTChanIO
    let runSrc src = C.runConduit $ src .| C.mapM_C (atomically . writeTChan chan)
    _ <- C.lift $ concurrently (runSrc src1) (runSrc src2)
    let consume = do
            mx <- C.lift $ atomically (tryReadTChan chan)
            case mx of
                Just x  -> C.yield x >> consume
                Nothing -> pure ()
    consume