module Heco.Agent.LanguageTools.Archives where

import Heco.Data.MonoHFunctor (MonoHFunctor(ohmap))
import Heco.Data.LanguageTool
    ( addTool,
      emptyModuleBuilder,
      toModule,
      LanguageTool(..),
      LanguageToolModule,
      MaybeParamDesc,
      ParamDesc,
      Ret )
import Heco.Data.LanguageToolError (LanguageToolError)
import Heco.Data.Entity (EntityId (EntityId))
import Heco.Data.Entity.TH (deriveEntity)
import Heco.Data.Model (ModelName)
import Heco.Data.Collection (CollectionName)
import Heco.Effectful.DatabaseService
    ( SearchData(TextData, DenseVectorData),
      SearchOps(..),
      DatabaseService,
      addEntity,
      deleteEntities,
      getEntity,
      searchEntities,
      setEntity_ )
import Heco.Effectful.LanguageService (LanguageService, embed)

import Effectful ((:>), MonadIO (liftIO), IOE, Eff)
import Effectful.Error.Dynamic (Error)
import Effectful.Reader.Dynamic (Reader, ask, runReader)

import Data.Text (Text)
import Data.Text qualified  as T
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
import Control.Applicative ((<|>))

data ArchiveToolOps = ArchiveToolOps
    { embeddingModel :: ModelName
    , collectionName :: CollectionName }

data ArchiveEntity = ArchiveEntity
    { id :: Maybe EntityId
    , vector :: Maybe (VU.Vector Float)
    , content :: Maybe Text
    , metadata :: Maybe Value }
    deriving (Eq, Show, Generic, Default)

deriveEntity ''ArchiveEntity

type HasArchiveToolEs es = 
    ( IOE :> es
    , DatabaseService :> es
    , LanguageService :> es )

addArchive ::
    ( Reader ArchiveToolOps :> es
    , HasArchiveToolEs es )
    => LanguageTool es
        "add"
        "向「荏苒之境图书馆」添加档案"
        (ParamDesc "content" Text "档案内容" -> Ret Value)
addArchive = LanguageTool \content -> do
    ops <- ask @ArchiveToolOps
    embedding <- embed ops.embeddingModel content
    time <- liftIO getCurrentTime
    id <- addEntity ops.collectionName ArchiveEntity
        { id = Nothing
        , vector = Just embedding
        , content = Just content
        , metadata = Just [aesonQQ|{
            create_time: #{time},
            modify_time: #{time}
        }|] }
    pure [aesonQQ|{ status: "success", archive_id: #{id} }|]

getArchive ::
    ( Reader ArchiveToolOps :> es
    , HasArchiveToolEs es )
    => LanguageTool es
        "get"
        "在「荏苒之境图书馆」中使用档案ID来获取档案内容"
        (ParamDesc "id" Int "档案ID" -> Ret Value)
getArchive = LanguageTool \id -> do
    ops <- ask @ArchiveToolOps
    entity <- getEntity @ArchiveEntity ops.collectionName $ EntityId id
    case entity of
        Just (ArchiveEntity { content = Just content }) ->
            pure [aesonQQ|{ status: "success", result: #{content} }|]
        _ -> pure [aesonQQ|{ status: "not_found" }|]

modifyArchive ::
    ( Reader ArchiveToolOps :> es
    , HasArchiveToolEs es )
    => LanguageTool es
        "modify"
        "修改「荏苒之境图书馆」中的档案"
        (ParamDesc "id" Int "档案ID" -> ParamDesc "content" Text "档案的新内容" -> Ret Value)
modifyArchive = LanguageTool \id content -> do
    ops <- ask @ArchiveToolOps
    entity <- getEntity @ArchiveEntity ops.collectionName $ EntityId id
    case entity of
        Nothing -> pure [aesonQQ|{ status: "not_found" }|]
        Just e -> doModify ops content e
    where 
        doModify ops content entity = do
            embedding <- embed ops.embeddingModel content
            time <- liftIO getCurrentTime
            setEntity_ ops.collectionName entity
                { vector = Just embedding
                , content = Just content
                , metadata = Just $ updateMetadata time entity.metadata }
            pure [aesonQQ|{ status: "success" }|]

        updateMetadata time = \case
            Just (Object map) ->
                Object $ KeyMap.insert "create_time" (toJSON time) map
            _ -> [aesonQQ|{
                create_time: #{time},
                modify_time: #{time}
            }|]

removeArchive ::
    ( Reader ArchiveToolOps :> es
    , HasArchiveToolEs es )
    => LanguageTool es
        "remove"
        "从「荏苒之境图书馆」中删除档案"
        (ParamDesc "id" Int "档案ID" -> Ret Value)
removeArchive = LanguageTool \id -> do
    ops <- ask @ArchiveToolOps
    deleteEntities ops.collectionName [fmt|id=={id}|]
    pure [aesonQQ|{ status: "success" }|]

searchArchive ::
    ( Reader ArchiveToolOps :> es
    , HasArchiveToolEs es )
    => LanguageTool es
        "search"
        "在「荏苒之境图书馆」中搜索档案"
        ( ParamDesc "keywords" Text "关键词"
        -> MaybeParamDesc "exclude_ids" [Int] "在搜索结果中排除的档案ID（例如用于排除之前已经搜索到的档案）"
        -> MaybeParamDesc "limit" Int "搜索数量限制"
        -> Ret Value)
searchArchive = LanguageTool \keywords excludeIds limit -> do
    ops <- ask @ArchiveToolOps
    let searchOps = def
            { vectorField = "sparse_vector"
            , limit = limit <|> Just 10
            , filter = excludeIds >>= \ids -> Just $ "id NOT IN " <> T.pack (show ids) }
        searchData = V.singleton $ TextData keywords
    res <- searchEntities @ArchiveEntity ops.collectionName searchOps searchData
    pure [aesonQQ|{ count: #{V.length res}, results: #{res} }|]

fuzzySearchArchive ::
    ( Reader ArchiveToolOps :> es
    , HasArchiveToolEs es )
    => LanguageTool es
        "fuzzy_search"
        "在「荏苒之境图书馆」中模糊搜索档案"
        ( ParamDesc "fragments" (V.Vector Text) "用于进行模糊搜索的相似档案片段"
        -> MaybeParamDesc "exclude_ids" [Int] "在搜索结果中排除的档案ID（例如用于排除之前已经搜索到的档案）"
        -> MaybeParamDesc "limit" Int "搜索数量限制"
        -> Ret Value)
fuzzySearchArchive = LanguageTool \fragments excludeIds limit -> do
    ops <- ask @ArchiveToolOps
    let searchOps = def
            { limit = limit <|> Just 10
            , filter = excludeIds >>= \ids -> Just $ "id NOT IN " <> T.pack (show ids) }
    searchData <- traverse (fmap DenseVectorData . embed ops.embeddingModel) fragments
    res <- searchEntities @ArchiveEntity ops.collectionName searchOps searchData
    pure [aesonQQ|{ count: #{V.length res}, results: #{res} }|]

archivesToolModule ::
    ( HasArchiveToolEs es
    , Error LanguageToolError :> es )
    => ArchiveToolOps -> LanguageToolModule "archives" (Eff es)
archivesToolModule ops = emptyModuleBuilder
    & addTool addArchive
    & addTool removeArchive
    & addTool getArchive
    & addTool modifyArchive
    & addTool searchArchive
    & addTool fuzzySearchArchive
    & toModule & ohmap (runReader ops)