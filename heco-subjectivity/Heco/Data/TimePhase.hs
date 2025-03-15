module Heco.Data.TimePhase where

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
    | MemoryAction Text
    deriving (Eq, Show)

data ImmanantContent
    = SenseDataContent SenseData
    | ActionContent Action
    deriving (Eq, Show)

newtype TimePhase = TimePhase (Vector ImmanantContent)
    deriving (Eq, Show)

emptyTimePhase :: TimePhase
emptyTimePhase = TimePhase mempty