{-# OPTIONS_GHC -Wno-redundant-constraints #-}

{-|
Module: Heco.Data.TimePhase
Description: Temporal phases and the synthesis of time-consciousness

This module implements Husserl's analysis of temporal phases in time-consciousness.
A temporal phase represents a unified synthesis of immanent contents that consciousness
performs in constituting temporal objects and experiencing the flow of time.

== Philosophical Background

In Husserl's /On the Phenomenology of the Consciousness of Internal Time/, temporal
experience is not simply the perception of temporal objects "out there," but involves
a complex synthesis within consciousness itself. This synthesis creates temporal phases
that have a threefold structure:

* __Retention__: The "just-past" that is still retained in present consciousness
* __Primal Impression__: The knife-edge of the living present  
* __Protention__: The anticipatory horizon toward the "about-to-come"

== Temporal Synthesis

A 'TimePhase' represents the result of temporal synthesis—a unified collection of
immanent objects that consciousness has synthesized into a coherent temporal moment.
This is not a mere collection but a genuine synthesis that creates temporal unity.

For example, when listening to a melody:

* The currently sounding note (primal impression)
* The retained previous notes (retention)  
* The anticipated resolution (protention)

All these are synthesized into a single temporal phase that constitutes the
melody-as-currently-experienced.

== The Flow of Time

Temporal phases are not static but participate in the flow of time-consciousness.
Each phase flows from present into retention, creating the temporal horizon that
makes temporal objects possible. The 'Unique' identifier ensures that each phase
maintains its temporal identity through this flow.
-}

module Heco.Data.TimePhase where

import Heco.Data.Text (formatList)
import Heco.Data.MonoHFunctor (MonoHFunctor(..))
import Heco.Data.Immanent qualified as Immanent
import Heco.Data.Immanent (SomeImmanent(..), Immanent)

import Effectful (MonadIO(liftIO))

import Data.Text.Lazy qualified as TL
import Data.Vector (Vector)
import Data.Vector qualified as V
import Data.Unique (Unique, newUnique)
import Data.Data (Typeable)

-- | A temporal phase represents a unified synthesis of immanent contents within
-- time-consciousness. This is not merely a collection of objects but a genuine
-- temporal synthesis that consciousness performs in constituting temporal objects.
--
-- In Husserl's analysis, temporal phases are the fundamental units of time-consciousness.
-- Each phase contains:
--
-- * A unique temporal identity that persists through the flow of time
-- * A synthesis of immanent objects that are unified in this temporal moment
--
-- == Temporal Identity
--
-- The 'Unique' component ensures that each temporal phase maintains its identity
-- as it flows from present into retention. This is crucial because the same
-- phase will be experienced differently as it moves through the temporal horizon:
--
-- * As primal impression (living present)
-- * As primary retention (just-past)  
-- * As secondary retention (further past)
--
-- == Synthesis vs. Collection
--
-- A 'TimePhase' is not a mere collection but represents the synthetic activity
-- of consciousness. When listening to a melody, the individual notes are not
-- simply aggregated but synthesized into a unified temporal object—the melody
-- as currently experienced.
--
-- This synthesis creates temporal objects that have duration and temporal
-- structure, rather than being instantaneous points.
data TimePhase m = TimePhase Unique (Vector (SomeImmanent m))

instance (Monad m, Typeable m) => Show (TimePhase m) where
    show = TL.unpack . format

instance MonoHFunctor TimePhase where
    ohmap f (TimePhase unique immanents) =
        TimePhase unique $ V.map (ohmap f) immanents

-- | Create a new temporal phase with a unique identity.
--
-- This represents the synthetic activity of consciousness creating a new
-- temporal moment from a collection of immanent contents. The unique
-- identifier ensures temporal identity through the flow of time.
new :: MonadIO m => Vector (SomeImmanent m) -> m (TimePhase m)
new cs = TimePhase <$> liftIO newUnique <*> pure cs

-- | The temporal extent of a phase, measured by the number of synthesized contents.
--
-- In phenomenology, temporal phases have duration—they are not instantaneous
-- points but extended temporal syntheses. The length represents the richness
-- of the temporal synthesis.
length :: TimePhase m -> Int
length (TimePhase _ contents) = V.length contents

-- | Format a temporal phase for display, showing its synthetic structure.
--
-- The formatting reveals both the temporal identity and the specific immanent
-- contents that have been synthesized into this phase.
format :: (Monad m, Typeable m) => TimePhase m -> TL.Text
format (TimePhase _ contents) =
    "TimePhase " <> formatList (map Immanent.format $ V.toList contents)

-- | Enrich a temporal phase with additional immanent contents.
--
-- This represents the ongoing synthetic activity of consciousness as new
-- contents are integrated into the current temporal phase. In the living
-- present, consciousness continuously enriches the current phase with
-- new immanent objects.
--
-- The temporal identity (Unique) is preserved, indicating that this is
-- the same phase being enriched rather than a new phase being created.
insertImmanents :: Vector (SomeImmanent m) -> TimePhase m -> TimePhase m
insertImmanents newImmanents (TimePhase unique immanents) =
    TimePhase unique $ immanents <> newImmanents

-- | Look up a specific type of immanent object within a temporal phase.
--
-- This represents the selective attention that consciousness can direct
-- toward specific contents within a temporal synthesis. For example,
-- when listening to an orchestra, consciousness might focus specifically
-- on the violin melody while the full temporal phase contains all instruments.
--
-- Returns the first object of the specified type, reflecting the temporal
-- order of synthesis.
lookupImmanent :: (Immanent m i, Typeable i) => TimePhase m -> Maybe i
lookupImmanent (TimePhase _ contents) = find 0
    where
        contentCount = V.length contents
        find i
            | i >= contentCount = Nothing
            | otherwise =
                case Immanent.tryCast $ contents V.! i of
                    Just res -> Just res
                    Nothing -> find $ i + 1

-- | Look up all immanent objects of a specific type within a temporal phase.
--
-- This represents consciousness attending to all contents of a particular
-- type within the temporal synthesis. For example, finding all the musical
-- notes within a temporal phase that also contains emotional responses
-- and bodily sensations.
--
-- The vector preserves the temporal order of synthesis, which is crucial
-- for temporal objects like melodies where order matters.
lookupImmanents :: (Immanent m i, Typeable i) => TimePhase m -> Vector i
lookupImmanents (TimePhase _ contents) =
    V.mapMaybe Immanent.tryCast contents
