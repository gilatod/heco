module Heco.Data.TimePhase where

import Data.Text (Text)

data SenseData
    = VisualSenseData Text
    | AcousticSenseData Text
    | TactileSenseData Text
    | KinaestheticSenseData Text

data Action
    = StatementAction Text
    | QueryAction Text
    | WishAction Text
    | WillAction Text
    | ImaginativeAction Text
    | MemoryAction Text

data ImmanantContent
    = SenseDataContent SenseData
    | ActionContent Action

data TimePhase = TimePhase
    { contents :: [ImmanantContent] }

emptyTimePhase :: TimePhase
emptyTimePhase = TimePhase []