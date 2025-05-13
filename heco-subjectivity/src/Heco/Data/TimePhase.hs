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
import Data.Typeable (Typeable, typeRep, Proxy(..))
import Data.Typeable qualified as Typeable
import Pattern.Cast (Cast(..))

class Typeable c => ImmanantContent c where
    encodeImmanantContent :: c -> [Text]
    encodeImmanantContent _ = []
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
    show = TL.unpack . format

new :: IOE :> es => Vector AnyImmanantContent -> Eff es TimePhase
new cs = do
    u <- liftIO $ newUnique
    pure $ TimePhase u cs

length :: TimePhase -> Int
length (TimePhase _ contents) = V.length contents

format :: TimePhase -> TL.Text
format (TimePhase _ contents) =
    TLB.toLazyText $ "TimePhase " <> (formatList $ map formatImmanant $ V.toList contents)
    where
        formatImmanant (AnyImmanantContent @c c) =
            case encodeImmanantContent c of
                [] -> TLB.fromString $ show $ typeRep (Proxy :: Proxy c)
                cs -> formatList $ map TLB.fromText $ cs
        formatList l =
            let inner = mconcat $ intersperse ", " l
            in "[" <> inner <> "]"

getImmanantContent :: forall c. ImmanantContent c => TimePhase -> Maybe c
getImmanantContent (TimePhase _ contents) = find 0
    where
        contentCount = V.length contents
        find i
            | i >= contentCount = Nothing
            | otherwise =
                case contents V.! i of
                    AnyImmanantContent c ->
                        case Typeable.cast c :: Maybe c of
                            Just res -> Just res
                            Nothing -> find $ i + 1

getImmanantContents :: forall c. ImmanantContent c => TimePhase -> Vector c
getImmanantContents (TimePhase _ contents) =
    V.mapMaybe doGet contents
    where
        doGet (AnyImmanantContent c) = Typeable.cast c :: Maybe c