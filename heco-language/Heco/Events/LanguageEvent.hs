module Heco.Events.LanguageEvent where

import Heco.Data.Session (SessionToken)
import Heco.Data.Message (Message)
import Heco.Data.Embeddings (Embeddings)

data LanguageEvent
    = OnLanguageChunkReceived SessionToken Message
    | OnLanguageResponseReceived SessionToken Message
    | OnEmbeddingsReceived SessionToken Embeddings