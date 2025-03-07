module Heco.Effectful.LanguageService where

import Heco.Data.Session (SessionToken)
import Heco.Data.Model (ModelName)
import Heco.Data.Message (Message)
import Heco.Data.Embedding (Embedding)

import Effectful (Effect, Eff, (:>))
import Effectful.TH (makeEffect)

import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)

data LanguageService :: Effect where
    Chat :: SessionToken -> ModelName -> NonEmpty Message -> LanguageService m Message
    Embed :: SessionToken -> ModelName -> [Text] -> LanguageService m [Embedding]

makeEffect ''LanguageService

chat_ :: LanguageService :> es
    => SessionToken -> ModelName -> NonEmpty Message -> Eff es ()
chat_ token model msgs = chat token model msgs >> pure ()

embed_ :: LanguageService :> es
    => SessionToken -> ModelName -> [Text] -> Eff es ()
embed_ token model text = embed token model text >> pure ()