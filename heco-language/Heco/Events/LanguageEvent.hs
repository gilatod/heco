module Heco.Events.LanguageEvent where

import Heco.Data.Message (Message)
import Heco.Data.Embedding (Embedding)

import Data.Text (Text)
import Data.Vector (Vector)

data LanguageEvent
    = OnReasoningChunkReceived Text
    | OnUtteranceChunkReceived Text
    | OnMessageReceived Message
    | OnEmbeddingsReceived (Vector Text) (Vector Embedding)