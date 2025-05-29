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
import Data.Aeson (Object)

import Control.Monad (void)

data ChatOps = ChatOps
    { modelName :: ModelName
    , stream :: Bool
    , tools :: [FunctionSchema]
    , temperature :: Maybe Float
    , topP :: Maybe Float
    , maxToken :: Maybe Int
    , presencePenalty :: Maybe Float
    , frequencyPenalty :: Maybe Float
    , extra :: Object }

chatOps :: ModelName -> ChatOps
chatOps name = ChatOps
    { modelName = name
    , stream = True
    , tools = []
    , temperature = Nothing
    , topP = Nothing
    , maxToken = Nothing
    , presencePenalty = Nothing
    , frequencyPenalty = Nothing
    , extra = mempty }

chatOpsNoStream :: ModelName -> ChatOps
chatOpsNoStream name = ChatOps
    { modelName = name
    , stream = False
    , tools = []
    , temperature = Nothing
    , topP = Nothing
    , maxToken = Nothing
    , presencePenalty = Nothing
    , frequencyPenalty = Nothing
    , extra = mempty }

data LanguageService :: Effect where
    Chat :: ChatOps -> Vector Message -> LanguageService m Message
    Embed :: ModelName -> Text -> LanguageService m Embedding
    EmbedMany :: ModelName -> Vector Text -> LanguageService m (Vector Embedding)

makeEffect ''LanguageService

chat_ :: (HasCallStack, LanguageService :> es)
    => ChatOps -> Vector Message -> Eff es ()
chat_ model msgs = void $ chat model msgs

embed_ :: (HasCallStack, LanguageService :> es)
    => ModelName -> Text -> Eff es ()
embed_ model text = void $ embed model text

embedMany_ :: (HasCallStack, LanguageService :> es)
    => ModelName -> Text -> Eff es ()
embedMany_ model text = void $ embed model text