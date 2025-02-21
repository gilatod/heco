module Heco.Effectful.LanguageService where

import Heco.Data.Session (SessionToken)
import Heco.Data.Message (Message)
import Heco.Data.Embeddings (Embeddings)

import Effectful (Effect)
import Effectful.TH (makeEffect)

import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)

data LanguageService :: Effect where
    Chat :: SessionToken -> NonEmpty Message -> LanguageService m Message
    Embed :: SessionToken -> Text -> LanguageService m Embeddings

makeEffect ''LanguageService