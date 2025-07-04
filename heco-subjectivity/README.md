# Heco Subjectivity: Computational Phenomenology of Time-Consciousness

## Introduction

The `heco-subjectivity` module provides a computational framework for modeling the fundamental structures of temporal consciousness as described in Edmund Husserl's phenomenological investigations. This library translates core insights from phenomenology—particularly the analysis of time-consciousness, intentionality, and immanent experience—into precise Haskell types and effects.

Rather than merely simulating consciousness, this module offers a rigorous formal model that captures the essential structures Husserl identified in his analysis of how consciousness constitutes temporal objects and experiences the flow of time.

## Philosophical Foundations

### Husserlian Time-Consciousness

Edmund Husserl's groundbreaking analysis in *On the Phenomenology of the Consciousness of Internal Time* reveals that temporal experience is not simply the perception of temporal objects "out there," but rather involves a complex synthesis within consciousness itself. This synthesis has three essential moments:

**Retention (Primary Memory)**: The "just-past" that is still retained in present consciousness. When listening to a melody, the notes that have just sounded are not simply gone—they are retained as a "comet's tail" that gives the present note its melodic meaning. This is not recollective memory but the immediate retention that makes temporal objects possible.

**Primal Impression**: The knife-edge of the living present, the "now-point" where new content enters consciousness. This is not a static instant but a dynamic phase that is constantly flowing into retention.

**Protention (Primary Expectation)**: The anticipatory horizon that projects toward the "about-to-come." Consciousness doesn't simply receive the present passively but actively anticipates and projects forward, creating expectations that can be fulfilled or disappointed.

### The Intentional Structure

Consciousness, for Husserl, is always "consciousness-of-something"—it has an intentional structure that correlates the mental act (noesis) with its intended object (noema):

**Noesis**: The mental act itself—the "how" of consciousness. This includes perceiving, remembering, imagining, judging, and all the various ways consciousness can be directed toward objects.

**Noema**: The object-as-intended, the "what" of consciousness with all its determinations and sense. The noema is not the real object but the object precisely as it is meant and given in consciousness.

**Noematic Object**: The identical pole that persists through different intentional acts. When I see a house from different angles, the noematic object "house" remains identical while the noemata (the specific ways it appears) vary.

### Immanent vs. Transcendent

Husserl distinguishes between:

**Immanent Objects**: Objects that exist within consciousness itself—mental states, temporal phases, the flow of consciousness. These are given with absolute certainty.

**Transcendent Objects**: Objects that exist beyond consciousness but are intended by it—physical things, other minds, ideal objects. These are given through profiles and can always be otherwise than intended.

## Computational Mapping

This library provides a precise computational model of these phenomenological structures:

### Core Data Types

```haskell
-- Noematic structure: the intentional correlate
data Noema = Noema
    { sense :: Text                    -- The meaning or sense
    , objects :: [NoematicObject]      -- Identical object poles
    , determinations :: [Determination] -- Specific ways of being given
    }

-- Immanent objects: contents of consciousness
class Monad m => Immanent m i where
    noesis :: i -> m (Maybe i)  -- The mental act directed toward the object
    noema :: i -> Maybe Noema   -- The intentional correlate
```

### Temporal Synthesis

```haskell
-- A temporal phase: a synthesis of immanent contents
data TimePhase m = TimePhase Unique (Vector (SomeImmanent m))

-- The flow of time-consciousness
data ImmanentTime :: Effect where
    ProgressPresent :: ImmanentTime m ()           -- Flow from present to retention
    Present :: Vector (SomeImmanent m) -> ImmanentTime m ()  -- Fill the living present
    GetRetention :: ImmanentTime m (Vector (TimePhase m))    -- Access retained phases
```

## Phenomenological Examples

### The Melody Example

Consider Husserl's famous example of listening to a melody. When we hear a melody, we don't simply hear a sequence of isolated notes. Instead, consciousness performs a temporal synthesis:

1. **Present Note**: The currently sounding note fills the living present
2. **Retained Notes**: Previous notes are retained (not remembered) as a "comet's tail" that gives the present note its melodic significance
3. **Anticipated Resolution**: Consciousness projects forward, expecting certain harmonic or melodic resolutions

In our computational model:

```haskell
-- Each note as an immanent object
data MusicalNote = MusicalNote Pitch Duration

instance Immanent IO MusicalNote where
    noesis note = pure $ Just note  -- Simple identity noesis
    noema note = Just $ Noema 
        { sense = "musical-note"
        , objects = [noteObject]
        , determinations = [pitchDetermination, durationDetermination]
        }

-- Playing a melody
playMelody :: [MusicalNote] -> Eff '[ImmanentTime] ()
playMelody notes = do
    forM_ notes $ \note -> do
        presentOne note      -- Present the note to consciousness
        progressPresent      -- Flow into retention
```

The melody emerges not from the individual notes but from the temporal synthesis that consciousness performs, retaining the past notes while anticipating future ones.

### Reading and Understanding

When reading a sentence, meaning doesn't emerge word by word but through temporal synthesis:

```haskell
data Word = Word Text Meaning

-- Reading creates temporal synthesis of meaning
readSentence :: [Word] -> Eff '[ImmanentTime] Meaning
readSentence words = do
    forM_ words presentOne
    -- The sentence meaning emerges from the temporal synthesis
    -- of retained word-meanings in consciousness
```

## Philosophical Significance

### Temporal Constitution

This library models how consciousness *constitutes* temporal objects rather than simply perceiving them. A melody is not "out there" waiting to be heard—it is constituted through the temporal synthesis of consciousness. The computational model captures this constitutive activity.

### The Unity of Consciousness

How does consciousness unify diverse contents into coherent experiences? The `TimePhase` type models this synthesis, showing how multiple `SomeImmanent` objects are unified into a single temporal phase.

### Passive and Active Synthesis

The automatic flow from present to retention (`progressPresent`) models passive synthesis—the automatic temporal flow that consciousness performs without deliberate effort. Active synthesis would involve deliberate acts of attention, comparison, and judgment.

### Bounded Retention

The ring buffer implementation captures a crucial insight: consciousness cannot retain infinite past. There is a horizon of retention beyond which temporal phases are "lost" to primary retention (though they may be recoverable through recollective memory).

## Connections to Other Phenomenologists

### Heidegger's Temporality

While Husserl focuses on the structure of time-consciousness, Heidegger analyzes temporality as the fundamental structure of Being-in-the-world. The anticipatory structure (`protention`) connects to Heidegger's analysis of Being-toward-death and authentic temporality.

### Merleau-Ponty's Embodied Time

Merleau-Ponty emphasizes how temporal experience is always embodied. Future extensions of this library might model how bodily habits and motor intentionality shape temporal synthesis.

### Sartre's Temporal Ekstases

Sartre's analysis of past, present, and future as modes of being-for-itself resonates with the triadic structure modeled here, though Sartre emphasizes the nihilating activity of consciousness more than Husserl.

## Usage

### Basic Time-Consciousness

```haskell
import Heco.Effectful.ImmanentTime
import Heco.Effectful.ImmanentTime.RingBuffer

-- Create a temporal consciousness with bounded retention
runTemporalConsciousness :: Eff '[ImmanentTime, IOE] a -> IO a
runTemporalConsciousness = 
    runEff . runRingBufferImmanentTimeEx (RingBufferOps 10)

-- Present contents to consciousness
example :: Eff '[ImmanentTime] ()
example = do
    presentOne (SomeThought "I think")
    presentOne (SomePerception "I see red")
    progressPresent  -- Flow into retention
    
    retention <- getRetention
    -- retention now contains the temporal synthesis
```

### Intentional Acts

```haskell
import Heco.Effectful.Intentionality

-- Create noematic objects
example :: Eff '[Intentionality] ()
example = do
    obj1 <- newObject  -- Create a noematic object
    obj2 <- newObject  -- Create another
    -- These objects can be intended through different noeses
```

## Installation

Add to your `cabal.project`:

```
packages: heco-subjectivity
```

Or in your `.cabal` file:

```cabal
build-depends: heco-subjectivity
```

## Further Reading

- Husserl, Edmund. *On the Phenomenology of the Consciousness of Internal Time*
- Husserl, Edmund. *Ideas: General Introduction to Pure Phenomenology*
- Zahavi, Dan. *Husserl's Phenomenology*
- Gallagher, Shaun and Dan Zahavi. *The Phenomenological Mind*

## Contributing

This library represents an ongoing experiment in computational phenomenology. Contributions that deepen the philosophical foundations or extend the formal models are welcome.

## License

BSD-3-Clause
