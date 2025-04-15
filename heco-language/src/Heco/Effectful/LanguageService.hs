module Heco.Effectful.LanguageService where

import Heco.Data.Model (ModelName)
import Heco.Data.Message (Message)
import Heco.Data.Embedding (Embedding)
import Heco.Data.FunctionSchema (FunctionSchema)

import Effectful (Effect, Eff, (:>))
import Effectful.TH (makeEffect)
import Effectful.Dispatch.Dynamic (HasCallStack)

import Data.Text (Text)
import Data.Vector (Vector)

data ChatOps = ChatOps
    { modelName :: ModelName
    , providers :: Maybe [Text]
    , stream :: Bool
    , tools :: [FunctionSchema]
    , temperature :: Maybe Float
    , topP :: Maybe Float
    , maxToken :: Maybe Int
    , presencePenalty :: Maybe Float
    , frequencyPenalty :: Maybe Float }

chatOps :: ModelName -> ChatOps
chatOps name = ChatOps
    { modelName = name
    , providers = Nothing
    , stream = True
    , tools = []
    , temperature = Nothing
    , topP = Nothing
    , maxToken = Nothing
    , presencePenalty = Nothing
    , frequencyPenalty = Nothing }

chatOpsNoStream :: ModelName -> ChatOps
chatOpsNoStream name = ChatOps
    { modelName = name
    , providers = Nothing
    , stream = False
    , tools = []
    , temperature = Nothing
    , topP = Nothing
    , maxToken = Nothing
    , presencePenalty = Nothing
    , frequencyPenalty = Nothing }

data LanguageService :: Effect where
    Chat :: ChatOps -> Vector Message -> LanguageService m Message
    Embed :: ModelName -> Text -> LanguageService m Embedding
    EmbedMany :: ModelName -> Vector Text -> LanguageService m (Vector Embedding)

makeEffect ''LanguageService

chat_ :: (HasCallStack, LanguageService :> es)
    => ChatOps -> Vector Message -> Eff es ()
chat_ model msgs = chat model msgs >> pure ()

embed_ :: (HasCallStack, LanguageService :> es)
    => ModelName -> Text -> Eff es ()
embed_ model text = embed model text >> pure ()

embedMany_ :: (HasCallStack, LanguageService :> es)
    => ModelName -> Text -> Eff es ()
embedMany_ model text = embed model text >> pure ()