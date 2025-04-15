module Heco.Data.TimePhase where

import Effectful (IOE, (:>), Eff, MonadIO (liftIO))

import Data.List (intersperse)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as TLB
import Data.Vector (Vector)
import Data.Vector qualified as V
import Data.Unique (Unique, newUnique)
import Data.Typeable (Typeable)
import Pattern.Cast (Cast(..))

class Typeable c => ImmanantContent c where
    encodeImmanantContent :: c -> [Text]
    getImmanantContentAttributes :: c -> [(Text, Text)]
    getImmanantContentAttributes _ = []

joinImmanantContent :: ImmanantContent c => Text -> c -> Text
joinImmanantContent separator =
    T.concat . intersperse separator . encodeImmanantContent

data AnyImmanantContent = forall c. ImmanantContent c => AnyImmanantContent c

instance ImmanantContent c => Cast c AnyImmanantContent where
    cast c = AnyImmanantContent c

data TimePhase = TimePhase Unique (Vector AnyImmanantContent)

instance Show TimePhase where
    show (TimePhase _ contents) =
        TL.unpack $ TLB.toLazyText $ "TimePhase <" <> V.foldl accumulate mempty contents <> ">"
        where
            accumulate prev c = prev <> ", " <> format c
            format (AnyImmanantContent c) =
                TLB.fromString $ show $ encodeImmanantContent c

newTimePhase :: IOE :> es => Vector AnyImmanantContent -> Eff es TimePhase
newTimePhase cs = do
    u <- liftIO $ newUnique
    pure $ TimePhase u cs