{-# LANGUAGE DeriveAnyClass #-}

module Heco.Data.Immanant.Memory where

import Heco.Data.Entity (EntityId)
import Heco.Data.Entity.TH (deriveEntity)
import Heco.Data.TimePhase (ImmanantContent(..), AnyImmanantContent (AnyImmanantContent))

import Data.Time (UTCTime)
import Data.Text (Text)
import Data.Default (Default(..))
import Data.Vector.Unboxing qualified as VU
import Data.Typeable (cast)
import Data.List (intersperse)

import GHC.Generics (Generic)

data Memory = Memory
    { id :: Maybe EntityId
    , vector :: Maybe (VU.Vector Float)
    , content :: [Text]
    , time :: Maybe UTCTime }
    deriving (Eq, Show, Generic, Default)

instance ImmanantContent Memory where
    encodeImmanantContent m = "memory":[last m.content]
    getImmanantContentAttributes m = [("type", mconcat $ intersperse ":" $ init m.content)]

deriveEntity ''Memory

immanantContentToMemory :: ImmanantContent c => c -> Maybe Memory
immanantContentToMemory c =
    case cast c of
        Just mem -> Just mem
        _ -> case encodeImmanantContent c of
            [] -> Nothing
            contents -> Just $ def { content = contents }

anyImmanantContentToMemory :: AnyImmanantContent -> Maybe Memory
anyImmanantContentToMemory (AnyImmanantContent c) =
    immanantContentToMemory c