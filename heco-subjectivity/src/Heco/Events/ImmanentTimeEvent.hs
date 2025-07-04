{-|
Module: Heco.Events.ImmanentTimeEvent
Description: Events in the flow of time-consciousness

This module defines events that occur during the temporal synthesis of consciousness.
These events capture the dynamic aspects of time-consciousness—the ongoing flow,
enrichment, and transformation of temporal phases.

== Philosophical Background

Time-consciousness is not a static structure but a dynamic flow involving
continuous synthetic activity. Husserl's analysis reveals several key temporal
processes:

* __Enrichment__: The living present is continuously enriched with new contents
* __Retention__: Present phases flow into retention, forming the temporal horizon
* __Loss__: Phases eventually pass beyond the retention horizon

These events allow observers to track the temporal dynamics of consciousness
and respond to changes in the temporal flow.

== Temporal Dynamics

The events capture the essential temporal processes:

1. **Phase Enrichment**: New immanent contents are synthesized into the present
2. **Phase Retention**: The present flows into retention, creating temporal horizon
3. **Phase Loss**: Phases pass beyond retention capacity, modeling bounded memory

These dynamics are essential to understanding how consciousness constitutes
temporal objects through ongoing synthetic activity.
-}

module Heco.Events.ImmanentTimeEvent where

import Heco.Data.Immanent (SomeImmanent)
import Heco.Data.TimePhase (TimePhase)

import Data.Vector (Vector)

-- | Events that occur during the temporal synthesis of consciousness.
--
-- These events capture the dynamic aspects of time-consciousness, allowing
-- observers to track the ongoing temporal flow and synthetic activity.
-- Each event represents a significant moment in the temporal constitution
-- of consciousness.
data ImmanentTimeEvent
    = -- | A temporal phase has been enriched with new immanent contents.
      --
      -- This event occurs when consciousness synthesizes new contents into
      -- the living present. It represents the ongoing enrichment of temporal
      -- phases that makes temporal objects possible.
      --
      -- For example, when listening to a melody, each new note enriches
      -- the current temporal phase, contributing to the melody-as-currently-
      -- experienced.
      --
      -- The event includes both the enriched phase and the specific contents
      -- that were added, allowing observers to track the synthetic activity.
      forall m. OnTimePhaseEnriched (TimePhase m) (Vector (SomeImmanent m))
      
    | -- | A temporal phase has flowed from present into retention.
      --
      -- This event occurs when the temporal flow moves a phase from the
      -- living present into the retention horizon. This is the fundamental
      -- temporal process that creates the temporal horizon necessary for
      -- temporal objects.
      --
      -- The retained phase becomes part of the "comet's tail" that gives
      -- meaning to present experience. Without this retention, temporal
      -- objects like melodies would be impossible.
      forall m. OnTimePhaseRetented (TimePhase m)
      
    | -- | A temporal phase has been lost beyond the retention horizon.
      --
      -- This event occurs when a phase passes beyond the bounded capacity
      -- of retention and is lost to primary retention. This models the
      -- finite character of consciousness—it cannot retain infinite past.
      --
      -- The lost phase may still be recoverable through recollective memory,
      -- but it is no longer part of the immediate temporal horizon that
      -- gives meaning to present experience.
      --
      -- This event is important for understanding the temporal limits of
      -- consciousness and the difference between primary retention and
      -- recollective memory.
      forall m. OnTimePhaseLost (TimePhase m)
