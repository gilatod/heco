module Heco.Data.TimePhase where

import Heco.Data.Memory (Memory)
import Heco.Data.Noema (Noema)

import Data.Text (Text)
import Data.Vector (Vector)

data SenseData
    = VisualSenseData Text
    | AcousticSenseData Text
    | OlfactorySenseData Text
    | TactileSenseData Text
    | KinaestheticSenseData Text
    deriving (Eq, Show)

data Action
    = StatementAction Text
    | ReasoningAction Text
    | QueryAction Text
    | WishAction Text
    | WillAction Text
    | ImaginativeAction Text
    | MemoryAction Memory
    deriving (Eq, Show)

data ImmanantContent
    = SenseDataContent SenseData
    | ActionContent Action
    | NoemaContent Noema
    deriving (Eq, Show)

isSenseDataContent :: ImmanantContent -> Bool
isSenseDataContent = \case
    SenseDataContent _ -> True
    _ -> False

isActionContent :: ImmanantContent -> Bool
isActionContent = \case
    ActionContent _ -> True
    _ -> False

isNoemaContent :: ImmanantContent -> Bool
isNoemaContent = \case
    NoemaContent _ -> True
    _ -> False

newtype TimePhase = TimePhase (Vector ImmanantContent)
    deriving (Eq, Show)

emptyTimePhase :: TimePhase
emptyTimePhase = TimePhase mempty