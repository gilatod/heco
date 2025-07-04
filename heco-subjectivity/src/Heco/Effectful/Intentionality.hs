{-|
Module: Heco.Effectful.Intentionality
Description: The intentional structure of consciousness as an effect

This module provides an effect for modeling intentionality—the fundamental structure
of consciousness as always being "consciousness-of-something." In Husserl's analysis,
consciousness is never empty but always directed toward objects, whether real or ideal,
present or absent, existent or non-existent.

== Philosophical Background

Intentionality is the defining characteristic of consciousness in phenomenology.
Unlike physical objects that simply exist, consciousness has the unique property
of being directed toward objects. This directedness is not a relation between
two existing things but the very structure that makes consciousness what it is.

Franz Brentano first identified intentionality as the mark of the mental, but
Husserl developed it into a comprehensive analysis of consciousness. Every mental
act has an intentional structure:

* __Noesis__: The mental act itself (perceiving, remembering, imagining)
* __Noema__: The object-as-intended with its sense and determinations
* __Noematic Object__: The identical pole that persists through different acts

== Object Constitution

In phenomenology, objects are not simply "given" to consciousness but are
constituted through intentional acts. The 'NewObject' operation models this
constitutive activity—consciousness doesn't just find objects but creates
the noematic objects that serve as identical poles for intentional acts.

This is not creation ex nihilo but the constitutive activity that makes
objects available as objects for consciousness. Even when perceiving a
"real" physical object, consciousness constitutes the object-as-perceived
with its specific sense and determinations.

== Temporal Intentionality

Intentionality has an essential temporal structure. Objects are intended
not just in the present but through retention (past-directed intentionality)
and protention (future-directed intentionality). This temporal intentionality
is what makes temporal objects like melodies possible.
-}

module Heco.Effectful.Intentionality where

import Heco.Data.Noema (NoematicObject (NoematicObject))

import Effectful (Effect, Eff)
import Effectful.TH (makeEffect)
import Effectful.Dispatch.Dynamic (HasCallStack, reinterpret)
import Effectful.State.Static.Shared (evalState, state)
import Data.Tuple.Extra (dupe)

-- | The intentionality effect models the fundamental structure of consciousness
-- as always being directed toward objects.
--
-- This effect provides operations for the constitutive activity of consciousness—
-- the creation of noematic objects that serve as identical poles for intentional acts.
-- These objects are not "real" objects in the world but the objects-as-intended
-- that consciousness constitutes through its intentional activity.
--
-- == Object Constitution
--
-- The primary operation is 'NewObject', which models consciousness constituting
-- a new noematic object. This is not arbitrary creation but follows the essential
-- structure of intentionality—consciousness must have objects to be directed toward.
--
-- == Identity Through Time
--
-- Noematic objects maintain their identity through different intentional acts
-- and across time. The same object can be perceived, remembered, imagined, or
-- judged while remaining the identical pole of these different acts.
data Intentionality :: Effect where
    -- | Constitute a new noematic object through intentional activity.
    --
    -- This models the constitutive activity of consciousness creating objects
    -- that can serve as identical poles for intentional acts. The object is
    -- not created arbitrarily but through the essential structure of intentionality.
    --
    -- For example, when consciousness first encounters a new melody, it constitutes
    -- a noematic object "this-melody" that can then be intended through different
    -- acts: hearing it, remembering it, anticipating its continuation, etc.
    NewObject :: Intentionality m NoematicObject

makeEffect ''Intentionality

-- | Standard implementation of intentionality using sequential object creation.
--
-- This implementation models the constitutive activity of consciousness through
-- a simple sequential numbering scheme. Each new object receives a unique
-- identifier that preserves its identity through different intentional acts.
--
-- == Philosophical Justification
--
-- While the implementation is simple, it captures the essential phenomenological
-- insight that consciousness constitutes objects through its intentional activity.
-- The sequential numbering ensures that each object maintains its identity as
-- the identical pole of different intentional acts.
--
-- In a more sophisticated implementation, object constitution might involve
-- complex synthesis of sense-data, temporal synthesis, or intersubjective
-- constitution. But the essential structure—consciousness constituting objects
-- that persist as identical poles—remains the same.
runStandardIntentionality ::
    ( HasCallStack )
    => Eff (Intentionality : es) a
    -> Eff es a
runStandardIntentionality = reinterpret wrap \_ -> \case
    NewObject -> NoematicObject <$> state (dupe . (+1))
    where
        wrap = evalState (0 :: Int)
