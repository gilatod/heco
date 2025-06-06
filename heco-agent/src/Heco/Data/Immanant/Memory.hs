{-# LANGUAGE DeriveAnyClass #-}

module Heco.Data.Immanant.Memory where

import Heco.Data.Entity (EntityId)
import Heco.Data.Entity.TH (deriveEntity)
import Heco.Data.TimePhase (ImmanantContent(..), AnyImmanantContent(..))

import Data.Text (Text)
import Data.Default (Default(..))
import Data.Vector.Unboxing qualified as VU
import Data.Typeable (cast)
import Data.List (intersperse)
import Data.Aeson (Value)

import GHC.Generics (Generic)

data Memory = Memory
    { id :: Maybe EntityId
    , vector :: Maybe (VU.Vector Float)
    , content :: Text
    , metadata :: Maybe Value }
    deriving (Eq, Show, Generic)

instance Default Memory where
    def = Memory
        { id = Nothing
        , vector = Nothing
        , content = ""
        , metadata = Nothing }

instance ImmanantContent Memory where
    encodeImmanantContent m = "memory":[m.content]

deriveEntity ''Memory

immanantContentToMemory :: ImmanantContent c => c -> Maybe Memory
immanantContentToMemory c =
    case cast c of
        Just mem -> Just mem
        _ -> case encodeImmanantContent c of
            [] -> Nothing
            contents -> Just $ def { content = mconcat $ intersperse ":" contents }

anyImmanantContentToMemory :: AnyImmanantContent -> Maybe Memory
anyImmanantContentToMemory (AnyImmanantContent c) =
    immanantContentToMemory c