module Heco.Effectful.HTTP where

import Effectful (Eff, (:>), IOE, MonadIO (liftIO))
import Effectful.Reader.Static (runReader, Reader)

import Network.HTTP.Client
    ( responseTimeoutMicro,
      newManager,
      ManagerSettings(managerResponseTimeout),
      Manager,
      responseTimeoutNone )
import Network.HTTP.Client.TLS (tlsManagerSettings)

makeHttpManager :: Maybe Int -> IO Manager
makeHttpManager timeout =
    newManager tlsManagerSettings
        { managerResponseTimeout =
            maybe responseTimeoutNone
                (\timeout -> responseTimeoutMicro $ timeout * 1000000)
                timeout }

evalHttpManager ::
    IOE :> es => Maybe Int -> Eff (Reader Manager : es) b -> Eff es b
evalHttpManager timeout e = do
    manager <- liftIO $ makeHttpManager timeout
    runReader manager e