{-# LANGUAGE QuasiQuotes #-}
module Heco.Agent.LanguageTools.Archives where

import Heco.Data.LanguageTool (LanguageTool(..), ParamDesc, Ret, LanguageToolSpec, MaybeParamDesc)
import Heco.Data.LanguageToolError (LanguageToolError)
import Heco.Data.Entity (EntityId)
import Heco.Data.Entity.TH (deriveEntity)
import Heco.Data.Model (ModelName)
import Heco.Data.Collection (CollectionName)
import Heco.Effectful.DatabaseService (DatabaseService, loadCollection, addEntity, deleteEntities, searchEntities, SearchOps(..), SearchData(..))
import Heco.Effectful.LanguageService (LanguageService, embed)

import Effectful ((:>), MonadIO (liftIO), IOE)
import Effectful.Error.Dynamic (Error)
import Effectful.Reader.Dynamic (Reader, ask)

import Data.Text (Text)
import Data.Vector qualified as V
import Data.Vector.Unboxing qualified as VU
import Data.Aeson (Value)
import Data.Aeson.QQ (aesonQQ)
import Data.Default (Default (def))
import Data.Time (getCurrentTime)
import Data.Maybe (fromMaybe)

import PyF (fmt)

import GHC.Generics (Generic)
import Pattern.Cast (Cast(cast))

data ArchivesToolOps = ArchivesToolOps
    { embeddingModel :: ModelName
    , collectionName :: CollectionName }

data ArchiveEntity = ArchiveEntity
    { id :: Maybe EntityId
    , vector :: Maybe (VU.Vector Float)
    , content :: Maybe Text
    , metadata :: Maybe Value }
    deriving (Eq, Show, Generic, Default)

deriveEntity ''ArchiveEntity

addArchive ::
    ( Reader ArchivesToolOps :> es
    , IOE :> es
    , DatabaseService :> es
    , LanguageService :> es )
    => LanguageTool es
        "add_archive"
        "向「荏苒之境图书馆」添加档案"
        (ParamDesc "content" Text "档案内容" -> Ret Value)
addArchive = LanguageTool \content -> do
    ops <- ask @ArchivesToolOps
    embedding <- embed ops.embeddingModel content
    time <- liftIO getCurrentTime

    loadCollection ops.collectionName
    id <- addEntity ops.collectionName ArchiveEntity
        { id = Nothing
        , vector = Just embedding
        , content = Just content
        , metadata = Just [aesonQQ|{
            create_time: #{time},
            modify_time: #{time}
            }|] }
    pure [aesonQQ|{ status: "success", archive_id: #{id} }|]

removeArchive ::
    ( Reader ArchivesToolOps :> es
    , DatabaseService :> es )
    => LanguageTool es
        "remove_archive"
        "从「荏苒之境图书馆」中删除档案"
        (ParamDesc "id" Int "档案ID" -> Ret Value)
removeArchive = LanguageTool \id -> do
    ops <- ask @ArchivesToolOps
    deleteEntities ops.collectionName [fmt|id=={id}|]
    pure [aesonQQ|{ status: "success" }|]

searchArchive ::
    ( Reader ArchivesToolOps :> es
    , DatabaseService :> es )
    => LanguageTool es
        "search_archive"
        "在「荏苒之境图书馆」中搜索档案"
        ( ParamDesc "keywords" Text "关键词"
        -> MaybeParamDesc "limit" Int "搜索数量限制"
        -> Ret Value)
searchArchive = LanguageTool \keywords limit -> do
    ops <- ask @ArchivesToolOps
    let searchOps = def
            { vectorField = "sparse_vector"
            , limit = Just $ fromMaybe 10 limit }
        searchData = V.singleton $ TextData keywords
    res <- searchEntities @ArchiveEntity ops.collectionName searchOps searchData
    pure [aesonQQ|{ results: #{res} }|]

archiveTools :: forall es.
    ( IOE :> es
    , DatabaseService :> es
    , LanguageService :> es
    , Reader ArchivesToolOps :> es
    , Error LanguageToolError :> es )
    => [LanguageToolSpec es]
archiveTools =
    [ cast $ addArchive @es
    , cast $ removeArchive @es
    , cast $ searchArchive @es ]