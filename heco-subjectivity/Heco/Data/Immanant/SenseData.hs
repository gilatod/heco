module Heco.Data.Immanant.SenseData where

import Heco.Data.TimePhase (ImmanantContent(..))

import Data.Text (Text)

data SenseData
    = VisualData Text
    | AcousticData Text
    | OlfactoryData Text
    | TactileData Text
    | KinaestheticData Text
    deriving (Eq, Show)

instance ImmanantContent SenseData where
    encodeImmanantContent = \case
        VisualData c -> ["visual", c]
        AcousticData c -> ["acoustic", c]
        OlfactoryData c -> ["olfactory", c]
        TactileData c -> ["tactile", c]
        KinaestheticData c -> ["kinaesthetic", c]