{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# LANGUAGE FunctionalDependencies #-}

{-|
Module: Heco.Data.Immanent
Description: Immanent objects and the structure of consciousness

This module defines the fundamental structure of immanent objects—objects that exist
within consciousness itself rather than in the transcendent world. In Husserl's
phenomenology, immanent objects are given with absolute certainty because they
are the very contents of consciousness.

== Philosophical Background

Husserl distinguishes between two fundamental types of objects:

* __Immanent Objects__: Objects that exist within consciousness itself—mental states,
  temporal phases, the flow of consciousness. These are given with absolute certainty.

* __Transcendent Objects__: Objects that exist beyond consciousness but are intended
  by it—physical things, other minds, ideal objects. These are given through profiles
  and can always be otherwise than intended.

The 'Immanent' class captures objects that can undergo noetic acts—that is, consciousness
can be directed toward them. Every immanent object has the potential for noesis (the
mental act) and may have a noematic correlate (the object-as-intended).

== The Noesis-Noema Correlation

In Husserl's analysis, every intentional act has a dual structure:

* __Noesis__: The mental act itself—the "how" of consciousness (perceiving, remembering, etc.)
* __Noema__: The object-as-intended—the "what" with its sense and determinations

The 'noesis' method models the mental act directed toward the immanent object, while
'noema' provides the intentional correlate when it exists.

== Temporal Immanence

In time-consciousness, immanent objects have a special temporal structure. They exist
within the flow of consciousness and participate in the temporal synthesis that
constitutes temporal objects like melodies, sentences, or experiences.
-}

module Heco.Data.Immanent where

import Heco.Data.MonoHFunctor (MonoHFunctor(..))
import Heco.Data.Noema (Noema)

import Data.Text.Lazy qualified as TL
import Data.Typeable (Proxy(Proxy), Typeable, typeRep)
import Data.Typeable qualified as Typeable
import Data.Functor.Identity (Identity (runIdentity))

import Pattern.Cast (Cast(..))

-- | The fundamental class for objects that exist within consciousness itself.
-- 
-- In Husserl's phenomenology, immanent objects are distinguished from transcendent
-- objects by their mode of givenness. Immanent objects are given with absolute
-- certainty because they are the very contents of consciousness, while transcendent
-- objects are given through profiles and can always be otherwise than intended.
--
-- Every immanent object can undergo noetic acts—consciousness can be directed
-- toward it. The functional dependency @i -> m@ ensures that each immanent object
-- type is associated with a specific monad that represents the context of consciousness.
--
-- == Examples of Immanent Objects
--
-- * Mental states (thoughts, emotions, sensations)
-- * Temporal phases of consciousness  
-- * Memory contents as retained
-- * Anticipated contents as protended
-- * The flow of consciousness itself
--
-- == The Noetic Act
--
-- The 'noesis' method represents the mental act directed toward the immanent object.
-- This could be:
--
-- * Reflection on a mental state
-- * Retention of a temporal phase
-- * Attention directed toward a sensation
-- * Recollection of a memory
--
-- The result @m (Maybe i)@ allows for the possibility that the noetic act might
-- fail (returning 'Nothing') or transform the object in some way.
--
-- == The Noematic Correlate
--
-- The 'noema' method provides the intentional correlate when it exists. Not all
-- immanent objects have noematic structure—some might be pure temporal flow
-- or undifferentiated sensation. When a noema exists, it specifies how the
-- object is intended with its sense and determinations.
class Monad m => Immanent m i | i -> m where
    -- | The noetic act directed toward this immanent object.
    -- 
    -- This represents consciousness being directed toward the object in some specific way.
    -- The default implementation is simple identity, but specific instances might
    -- implement more complex noetic transformations.
    --
    -- For example, when consciousness reflects on a thought, the thought might
    -- be transformed through the reflective act, gaining new determinations
    -- or changing its temporal character.
    noesis :: i -> m (Maybe i)
    noesis = pure . Just

    -- | The noematic correlate of this immanent object, if it exists.
    --
    -- This specifies how the object is intended when consciousness is directed
    -- toward it. The noema includes the sense, noematic objects, and determinations
    -- that constitute the object-as-intended.
    --
    -- Some immanent objects (like pure temporal flow) might not have noematic
    -- structure and return 'Nothing'.
    noema :: i -> Maybe Noema
    noema _ = Nothing

-- | An existential wrapper for immanent objects that allows heterogeneous
-- collections while preserving the phenomenological structure.
--
-- In consciousness, we often have multiple different types of immanent objects
-- present simultaneously—thoughts, sensations, memories, anticipations. The
-- 'SomeImmanent' type allows us to collect these into unified temporal phases
-- while preserving their individual noetic and noematic structure.
--
-- == Monad Mapping
--
-- The 'MappedImmanent' constructor allows immanent objects from one monadic
-- context to be lifted into another. This is crucial for temporal synthesis,
-- where objects from past phases (in different temporal contexts) need to
-- be retained in the present phase.
--
-- For example, a memory from yesterday's consciousness can be mapped into
-- today's consciousness while preserving its immanent character.
data SomeImmanent m
    = forall i. (Immanent m i, Typeable i) => SomeImmanent i
      -- ^ A direct immanent object in the current monadic context
    | forall m' i. (Immanent m' i, Typeable i) => MappedImmanent (forall a. m' a -> m a) i
      -- ^ An immanent object mapped from another monadic context

instance Monad m => Immanent m (SomeImmanent m) where
    noesis (SomeImmanent i) = fmap SomeImmanent <$> noesis i
    noesis (MappedImmanent f i) = fmap (MappedImmanent f) <$> f (noesis i)

    noema (SomeImmanent i) = noema i
    noema (MappedImmanent _ i) = noema i

instance MonoHFunctor SomeImmanent where
    ohmap f (SomeImmanent i) = MappedImmanent f i
    ohmap f (MappedImmanent f' i) = MappedImmanent (f . f') i

instance (Immanent m i, Typeable i) => Cast i (SomeImmanent m) where
    cast = SomeImmanent

instance Show (SomeImmanent m) where
    show (SomeImmanent i) = TL.unpack $ format i
    show (MappedImmanent _ i) = TL.unpack $ format i

format :: forall m i. (Immanent m i, Typeable i) => i -> TL.Text
format i =
    case noema i of
        Nothing -> typeStr
        Just n -> typeStr <> "<" <> TL.pack (show n) <> ">"
    where
        typeStr = TL.pack $ show $ typeRep (Proxy :: Proxy i)

tryCast :: (Immanent m i, Typeable i) => SomeImmanent m -> Maybe i
tryCast (SomeImmanent i) = Typeable.cast i
tryCast (MappedImmanent _ i) = Typeable.cast i

fromPure :: (Immanent Identity i, Typeable i, Monad m) => i -> SomeImmanent m
fromPure = MappedImmanent (pure . runIdentity)
