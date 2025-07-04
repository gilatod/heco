{-|
Module: Heco.Data.Noema
Description: Noematic structures in phenomenological analysis

This module implements the noematic side of Husserl's intentional analysis. In phenomenology,
every intentional act (noesis) has its correlate in the intended object-as-intended (noema).
The noema is not the real object but the object precisely as it is meant, with all its
determinations and sense.

== Philosophical Background

In Husserl's /Ideas/, the noema is described as the "object-as-intended" that correlates
with every mental act. When I perceive a house, the noema is not the physical house but
the house-as-perceived, with all its specific determinations: "the red house," "the house
seen from this angle," "the house appearing in twilight," etc.

The noematic structure has several essential components:

* __Noematic Object__: The identical pole that persists through different acts
* __Noematic Sense__: The specific way the object is given or meant  
* __Determinations__: The specific qualities and characteristics as intended

== Temporal Considerations

In time-consciousness, noemata have a temporal structure. A melody-as-heard is not just
a collection of notes but a temporal noema that synthesizes retention, primal impression,
and protention into a unified musical object.
-}

module Heco.Data.Noema where

import Data.Text (Text)
import Data.Hashable (Hashable)

import Pattern.Cast (Cast(..))

-- | A noematic object represents the identical pole that persists through different
-- intentional acts. When I see a house from different angles, hear a melody through
-- different phases, or remember an event at different times, the noematic object
-- remains identical while the specific noemata (ways of being given) vary.
--
-- In Husserl's analysis, the noematic object is not the real object but the 
-- object-as-intended that serves as the identical pole of reference for 
-- consciousness. It provides the unity that allows consciousness to recognize
-- "the same" object through different presentations.
--
-- The 'Int' representation allows for efficient identity tracking while maintaining
-- the phenomenological insight that objects are constituted through intentional acts.
newtype NoematicObject = NoematicObject Int
    deriving (Show, Eq, Ord, Enum, Bounded)
    deriving Num via Int
    deriving newtype Hashable

instance Cast NoematicObject Int where
    cast (NoematicObject id) = id

-- | A determination specifies how an object is given in consciousness with a particular
-- content and degree of satisfaction. In phenomenology, objects are never given as
-- bare substrates but always with specific determinations.
--
-- For example, when perceiving a house, it might be determined as:
-- * "red" (color determination)  
-- * "two-story" (structural determination)
-- * "familiar" (recognition determination)
--
-- The 'satisfaction' field (0.0 to 1.0) represents how completely this determination
-- is fulfilled in the current intentional act. A house seen in fog might have low
-- satisfaction for color determinations but high satisfaction for shape determinations.
--
-- This captures Husserl's insight that intentional acts can be more or less "fulfilled"
-- - they can be satisfied by intuitive presentation or remain as empty intentions.
data Determination = Determination
    { content :: Text      -- ^ The specific determination content
    , satisfaction :: Float -- ^ Degree of fulfillment (0.0 = empty, 1.0 = fully intuitive)
    } deriving (Eq, Show)

-- | The complete noematic correlate of an intentional act. Every mental act 
-- (perceiving, remembering, imagining, etc.) has its noematic correlate that
-- specifies exactly how the object is intended.
--
-- The noema consists of:
--
-- * __Sense__: The overall meaning or sense through which the object is intended
-- * __Objects__: The noematic objects that serve as identical poles
-- * __Determinations__: The specific ways the objects are given
--
-- For example, when listening to Beethoven's 9th Symphony:
-- * Sense: "Beethoven's 9th Symphony as currently being heard"
-- * Objects: [symphony-object, movement-objects, theme-objects]  
-- * Determinations: [tempo-determination, key-determination, emotional-determination]
--
-- The noema captures the complete intentional correlate without reference to
-- the real existence of the intended object. Whether the symphony is being
-- performed live, played from a recording, or remembered, each has its own
-- noematic structure.
data Noema = Noema
    { sense :: Text                    -- ^ The overall noematic sense
    , objects :: [NoematicObject]      -- ^ Identical object poles
    , determinations :: [Determination] -- ^ Specific determinations
    } deriving (Eq, Show)
