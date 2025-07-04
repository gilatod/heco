{-|
Module: Heco.Data.ImmanentTimeError
Description: Error conditions in time-consciousness

This module defines error conditions that can arise in the temporal synthesis
of consciousness. In phenomenology, consciousness is generally characterized
by its smooth temporal flow, but disruptions can occur that interrupt the
normal synthetic activity.

== Philosophical Background

While Husserl's analysis focuses on the normal functioning of time-consciousness,
phenomenology also recognizes that temporal synthesis can be disrupted. This
might occur in:

* Trauma, where the normal temporal flow is interrupted
* Altered states of consciousness
* Pathological conditions affecting temporal experience
* Computational limits in modeling temporal synthesis

The error types capture these disruptions while maintaining the phenomenological
framework of temporal consciousness.
-}

module Heco.Data.ImmanentTimeError where

import Control.Exception (Exception)

-- | Errors that can occur in the temporal synthesis of consciousness.
--
-- These errors represent disruptions in the normal flow of time-consciousness
-- that prevent the synthetic activity from proceeding. While consciousness
-- normally performs temporal synthesis smoothly, various conditions can
-- interrupt this process.
data ImmanentTimeError
    = -- | An unhandled error in temporal synthesis.
      --
      -- This represents a disruption in time-consciousness that cannot be
      -- resolved within the normal synthetic activity. It might correspond
      -- to computational limits, resource exhaustion, or other conditions
      -- that prevent temporal synthesis from proceeding.
      --
      -- In phenomenological terms, this might correspond to a breakdown
      -- in the temporal flow that requires intervention to restore normal
      -- time-consciousness.
      UnhandledImmanentTimeError String
    deriving (Eq, Show)

instance Exception ImmanentTimeError
