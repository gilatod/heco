{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}

module Heco.Effectful.HTTP where

import Effectful (Eff, (:>), IOE, MonadIO (liftIO))
import Effectful.Reader.Static (runReader, Reader)

import Network.Connection (TLSSettings(settingClientSupported))
import Network.HTTP.Client
    ( responseTimeoutMicro,
      newManager,
      ManagerSettings(managerResponseTimeout),
      Manager,
      responseTimeoutNone )
import Network.HTTP.Client.TLS (mkManagerSettings)

import Network.TLS
    ( Supported(supportedExtendedMainSecret), EMSMode(AllowEMS) )

import Data.Default (def)

makeHttpManager :: Maybe Int -> IO Manager
makeHttpManager timeout = do
    let tlsSettings = def
            { settingClientSupported = def
                { supportedExtendedMainSecret = AllowEMS } }
        tlsManagerSettings = (mkManagerSettings tlsSettings Nothing)
            { managerResponseTimeout =
                maybe responseTimeoutNone
                    (\timeout -> responseTimeoutMicro $ timeout * 1000000)
                    timeout }
    newManager tlsManagerSettings

evalHttpManager ::
    IOE :> es => Maybe Int -> Eff (Reader Manager : es) b -> Eff es b
evalHttpManager timeout e = do
    manager <- liftIO $ makeHttpManager timeout
    runReader manager e