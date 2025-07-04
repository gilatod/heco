{-|
Module: Heco.Effectful.ImmanentTime
Description: Time-consciousness as an effectful computation

This module implements Husserl's analysis of time-consciousness as an effect system.
Time-consciousness is not simply the perception of temporal objects but the fundamental
structure that makes temporal experience possible. It involves the synthesis of
retention, primal impression, and protention into unified temporal objects.

== Philosophical Background

In Husserl's /On the Phenomenology of the Consciousness of Internal Time/, temporal
experience has a complex structure that cannot be reduced to a sequence of "now-points."
Instead, consciousness performs a continuous temporal synthesis with three essential
moments:

* __Retention (Primary Memory)__: The "just-past" that is still retained in present
  consciousness, forming a "comet's tail" that gives temporal objects their meaning

* __Primal Impression__: The knife-edge of the living present where new content
  enters consciousness

* __Protention (Primary Expectation)__: The anticipatory horizon that projects
  toward the "about-to-come"

== The Flow of Time

Time-consciousness involves a continuous flow where the present constantly becomes
past through retention. This is not mechanical succession but a synthetic activity
that creates temporal objects with duration and temporal structure.

The 'ProgressPresent' operation models this flow, moving the current temporal phase
into retention and creating space for new temporal synthesis.

== Temporal Synthesis

The 'Present' operation models how consciousness synthesizes immanent contents into
unified temporal phases. This is not mere aggregation but genuine synthesis that
creates temporal objects like melodies, sentences, or experiences.

== Bounded Retention

Consciousness cannot retain infinite past—there is a horizon of retention beyond
which temporal phases are lost to primary retention. This bounded character is
essential to the structure of time-consciousness.
-}

module Heco.Effectful.ImmanentTime where

import Heco.Data.Immanent (SomeImmanent(..), Immanent) 
import Heco.Data.TimePhase (TimePhase)

import Effectful (Effect, Eff, (:>))
import Effectful.TH (makeEffect)
import Effectful.Dispatch.Dynamic (HasCallStack)

import Data.Vector (Vector)
import Data.Vector qualified as V
import Data.Data (Typeable)

-- | The immanent time effect models the fundamental structure of time-consciousness.
--
-- This effect provides operations for the temporal synthesis that consciousness
-- performs in constituting temporal objects and experiencing the flow of time.
-- It captures Husserl's insight that temporal experience is not passive reception
-- but active synthesis.
--
-- == The Living Present
--
-- The present is not a static instant but a dynamic temporal phase that is
-- continuously enriched with new immanent contents and flows into retention.
--
-- == Retention Horizon
--
-- Retained temporal phases form the temporal horizon that gives meaning to
-- present experience. Without retention, temporal objects like melodies
-- would be impossible—each note would be isolated rather than part of a
-- temporal synthesis.
--
-- == Temporal Flow
--
-- The flow of time is modeled through 'ProgressPresent', which moves the
-- current phase into retention and creates space for new temporal synthesis.
data ImmanentTime :: Effect where
    -- | Progress the flow of time, moving present into retention.
    --
    -- This models the fundamental temporal flow of consciousness where the
    -- living present constantly becomes past through retention. This is not
    -- mechanical succession but the synthetic activity that creates temporal
    -- objects with duration.
    --
    -- The operation enriches the current present through noetic acts before
    -- moving it into retention, modeling how consciousness actively synthesizes
    -- temporal contents rather than passively receiving them.
    ProgressPresent :: ImmanentTime m ()
    
    -- | Present immanent contents to consciousness for temporal synthesis.
    --
    -- This models how consciousness receives and synthesizes immanent contents
    -- into the living present. The contents are not simply added but integrated
    -- into the ongoing temporal synthesis that constitutes temporal objects.
    --
    -- For example, when hearing a melody, each note is presented to consciousness
    -- and synthesized with the retained previous notes to constitute the
    -- melody-as-currently-experienced.
    Present :: Vector (SomeImmanent m) -> ImmanentTime m ()
    
    -- | Access the current temporal phase of the living present.
    --
    -- This provides access to the ongoing temporal synthesis that consciousness
    -- is currently performing. The present phase contains all the immanent
    -- contents that have been synthesized into the current temporal moment.
    GetPresent :: ImmanentTime m (TimePhase m)
    
    -- | Access the retention horizon of past temporal phases.
    --
    -- This provides access to the retained temporal phases that form the
    -- temporal horizon of consciousness. These retained phases are not
    -- recollective memories but the primary retention that makes temporal
    -- objects possible.
    --
    -- The vector is ordered from oldest to newest, reflecting the temporal
    -- flow from present into retention.
    GetRetention :: ImmanentTime m (Vector (TimePhase m))
    
    -- | Get the current length of the retention horizon.
    --
    -- This indicates how many temporal phases are currently retained in
    -- primary retention. The length varies as phases flow into retention
    -- and are eventually lost beyond the retention horizon.
    GetRetentionLength :: ImmanentTime m Int
    
    -- | Get the maximum capacity of the retention horizon.
    --
    -- This reflects the bounded character of retention—consciousness cannot
    -- retain infinite past. Beyond this capacity, temporal phases are lost
    -- to primary retention (though they may be recoverable through
    -- recollective memory).
    GetRetentionCapacity :: ImmanentTime m Int
    
    -- | Clear the retention horizon.
    --
    -- This models a disruption in the temporal flow of consciousness, such
    -- as might occur in trauma, meditation, or other altered states where
    -- the normal temporal synthesis is interrupted.
    ClearRetention :: ImmanentTime m ()

makeEffect ''ImmanentTime

-- | Present a list of immanent contents to consciousness.
--
-- This convenience function converts a list to a vector for presentation
-- to consciousness. It models how consciousness can receive multiple
-- contents simultaneously for temporal synthesis.
presentList ::
    ImmanentTime :> es
    => [SomeImmanent (Eff es)] -> Eff es ()
presentList contents = present $ V.fromList contents

-- | Present a single immanent object to consciousness.
--
-- This models the presentation of individual immanent contents to consciousness
-- for integration into the temporal synthesis. The object is wrapped in
-- 'SomeImmanent' to allow heterogeneous temporal synthesis.
presentOne ::
    (ImmanentTime :> es, Immanent (Eff es) i, Typeable i)
    => i -> Eff es ()
presentOne content = present $ V.singleton $ SomeImmanent content

-- | Get the most recently retained temporal phase.
--
-- This provides access to the temporal phase that has most recently flowed
-- from present into retention. It represents the immediate temporal horizon
-- that gives context to present experience.
--
-- Returns 'Nothing' if no phases have been retained yet, indicating that
-- consciousness is in its initial state without temporal horizon.
getLatestRetent ::
    (HasCallStack, ImmanentTime :> es)
    => Eff es (Maybe (TimePhase (Eff es)))
getLatestRetent = do
    retention <- getRetention
    if V.length retention == 0
        then pure Nothing
        else pure $ Just $ retention V.! (V.length retention - 1)
