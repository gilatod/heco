module Heco.Agent.LanguageTools.Downloader where

import Heco.Data.LanguageTool (LanguageTool(..), ParamDesc, Ret, MaybeParamDesc, LanguageToolModule, addTool)
import Heco.Data.LanguageToolError (LanguageToolError)
import Heco.Data.Entity (EntityId (EntityId))
import Heco.Data.Entity.TH (deriveEntity)
import Heco.Data.Model (ModelName)
import Heco.Data.Collection (CollectionName)
import Heco.Effectful.DatabaseService (DatabaseService, addEntity, deleteEntities, searchEntities, SearchOps(..), SearchData(..), getEntity, setEntity_)
import Heco.Effectful.LanguageService (LanguageService, embed)

import Effectful ((:>), MonadIO (liftIO), IOE)
import Effectful.Error.Dynamic (Error)
import Effectful.Reader.Dynamic (Reader, ask)

import Data.Text (Text)
import Data.Vector qualified as V
import Data.Vector.Unboxing qualified as VU
import Data.Aeson (Value (Object), ToJSON (toJSON))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.QQ (aesonQQ)
import Data.Default (Default (def))
import Data.Time (getCurrentTime)
import Data.Maybe (fromMaybe)
import Data.Function ((&))

import PyF (fmt)
import GHC.Generics (Generic)