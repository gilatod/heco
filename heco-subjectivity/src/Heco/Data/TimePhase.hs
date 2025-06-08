module Heco.Data.TimePhase where

import Effectful (IOE, (:>), Eff, MonadIO (liftIO))

import Data.List (intersperse)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Vector (Vector)
import Data.Vector qualified as V
import Data.Unique (Unique, newUnique)
import Data.Typeable (Typeable, typeRep, Proxy(..))
import Data.Typeable qualified as Typeable

import Pattern.Cast (Cast(..))

formatList :: [TL.Text] -> TL.Text
formatList l =
    let inner = mconcat $ intersperse ", " l
    in "[" <> inner <> "]"

class Typeable c => ImmanantContent c where
    encodeImmanantContent :: c -> [Text]
    encodeImmanantContent _ = []
    getImmanantContentAttributes :: c -> [(Text, Text)]
    getImmanantContentAttributes _ = []

joinImmanantContent :: ImmanantContent c => Text -> c -> Text
joinImmanantContent separator =
    T.concat . intersperse separator . encodeImmanantContent

formatImmanantContent :: forall c. ImmanantContent c => c -> TL.Text
formatImmanantContent c =
    case encodeImmanantContent c of
        [] -> TL.pack $ show $ typeRep (Proxy :: Proxy c)
        cs -> formatList $ map TL.fromStrict cs

data SomeImmanantContent = forall c. ImmanantContent c => SomeImmanantContent c

instance Show SomeImmanantContent where
    show (SomeImmanantContent c) = TL.unpack $ formatImmanantContent c

instance ImmanantContent c => Cast c SomeImmanantContent where
    cast = SomeImmanantContent

data TimePhase = TimePhase Unique (Vector SomeImmanantContent)

instance Show TimePhase where
    show = TL.unpack . format

new :: IOE :> es => Vector SomeImmanantContent -> Eff es TimePhase
new cs = do
    u <- liftIO newUnique
    pure $ TimePhase u cs

length :: TimePhase -> Int
length (TimePhase _ contents) = V.length contents

format :: TimePhase -> TL.Text
format (TimePhase _ contents) =
    "TimePhase " <> formatList (map format $ V.toList contents)
    where
        format (SomeImmanantContent c) = formatImmanantContent c

castImmanantContent :: ImmanantContent c => SomeImmanantContent -> Maybe c
castImmanantContent (SomeImmanantContent c) = Typeable.cast c

getImmanantContent :: forall c. ImmanantContent c => TimePhase -> Maybe c
getImmanantContent (TimePhase _ contents) = find 0
    where
        contentCount = V.length contents
        find i
            | i >= contentCount = Nothing
            | otherwise =
                case castImmanantContent $ contents V.! i of
                    Just res -> Just res
                    Nothing -> find $ i + 1

getImmanantContents :: forall c. ImmanantContent c => TimePhase -> Vector c
getImmanantContents (TimePhase _ contents) =
    V.mapMaybe castImmanantContent contents