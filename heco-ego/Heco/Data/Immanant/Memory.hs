module Heco.Data.Immanant.Memory where

import Heco.Data.Entity (EntityId)
import Heco.Data.Entity.TH (deriveEntity)
import Heco.Data.TimePhase (ImmanantContent(..), AnyImmanantContent (AnyImmanantContent))

import Data.Time (UTCTime)
import Data.Text (Text)
import Data.Default (Default(..))
import Data.Vector.Unboxing qualified as VU
import Data.Typeable (cast)

import GHC.Generics (Generic)

data Memory = Memory
    { id :: Maybe EntityId
    , vector :: Maybe (VU.Vector Float)
    , content :: [Text]
    , time :: Maybe UTCTime }
    deriving (Show, Generic)

instance Default Memory where
    def = Memory
        { id = Nothing
        , vector = Nothing
        , content = []
        , time = Nothing }

instance ImmanantContent Memory where
    encodeImmanantContent m = "memory":m.content

deriveEntity ''Memory

immanantContentToMemory :: ImmanantContent c => c -> Memory
immanantContentToMemory c =
    case cast c of
        Just mem -> mem
        _ -> def { content = encodeImmanantContent c }

anyImmanantContentToMemory :: AnyImmanantContent -> Memory
anyImmanantContentToMemory (AnyImmanantContent c) =
    immanantContentToMemory c