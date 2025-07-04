# Heco Subjectivity Tutorial: Computational Phenomenology in Practice

This tutorial demonstrates how to use the `heco-subjectivity` library to model fundamental structures of consciousness and temporal experience. We'll work through concrete examples that illustrate key phenomenological concepts.

## Prerequisites

This tutorial assumes familiarity with:
- Basic Haskell programming
- The `effectful` library for effect systems
- Elementary phenomenological concepts (covered in the main README)

## Setup

First, ensure you have the library available:

```haskell
import Heco.Data.Noema
import Heco.Data.Immanent
import Heco.Data.TimePhase
import Heco.Effectful.ImmanentTime
import Heco.Effectful.Intentionality
import Heco.Effectful.ImmanentTime.RingBuffer

import Effectful
import Data.Vector qualified as V
```

## Example 1: The Melody - Temporal Synthesis in Action

Let's implement Husserl's famous melody example to understand temporal synthesis.

### Defining Musical Notes as Immanent Objects

```haskell
-- A musical note with pitch and duration
data MusicalNote = MusicalNote
    { pitch :: Text      -- e.g., "C4", "F#5"
    , duration :: Float  -- in seconds
    , timbre :: Text     -- e.g., "piano", "violin"
    } deriving (Show, Eq, Typeable)

-- Musical notes are immanent objects in consciousness
instance Immanent IO MusicalNote where
    -- Simple identity noesis - consciousness can always be directed toward a note
    noesis note = pure $ Just note
    
    -- The noematic structure of a musical note
    noema note = Just $ Noema
        { sense = "musical-note-as-heard"
        , objects = [noteObject]  -- The identical note-object
        , determinations = 
            [ Determination ("pitch:" <> note.pitch) 1.0
            , Determination ("duration:" <> T.pack (show note.duration)) 1.0
            , Determination ("timbre:" <> note.timbre) 0.8  -- might be less clear
            ]
        }
    where
        -- Create a noematic object for this note
        noteObject = NoematicObject $ hash (note.pitch, note.duration, note.timbre)
```

### Playing a Melody: Temporal Synthesis

```haskell
-- A simple melody: "Twinkle, Twinkle, Little Star" opening
twinkleMelody :: [MusicalNote]
twinkleMelody = 
    [ MusicalNote "C4" 0.5 "piano"  -- Twin-
    , MusicalNote "C4" 0.5 "piano"  -- kle
    , MusicalNote "G4" 0.5 "piano"  -- twin-
    , MusicalNote "G4" 0.5 "piano"  -- kle
    , MusicalNote "A4" 0.5 "piano"  -- lit-
    , MusicalNote "A4" 0.5 "piano"  -- tle
    , MusicalNote "G4" 1.0 "piano"  -- star
    ]

-- Play the melody through time-consciousness
playMelody :: [MusicalNote] -> Eff '[ImmanentTime, Intentionality, IOE] ()
playMelody notes = do
    putStrLn "Playing melody through time-consciousness..."
    
    forM_ (zip [1..] notes) $ \(i, note) -> do
        putStrLn $ "Note " <> show i <> ": " <> show note
        
        -- Present the note to consciousness
        presentOne note
        
        -- Show current temporal synthesis
        present <- getPresent
        putStrLn $ "Current temporal phase: " <> show present
        
        -- Progress time (note flows into retention)
        progressPresent
        
        -- Show retention horizon
        retention <- getRetention
        putStrLn $ "Retention horizon length: " <> show (V.length retention)
        putStrLn "---"

-- Run the melody example
runMelodyExample :: IO ()
runMelodyExample = runEff $ 
    runRingBufferImmanentTimeEx (RingBufferOps 5) $
    runStandardIntentionality $
    playMelody twinkleMelody
```

### Understanding the Output

When you run this example, you'll see how:

1. **Each note is presented** to consciousness as an immanent object
2. **Temporal synthesis occurs** - the note is integrated into the current temporal phase
3. **The present flows into retention** - creating the "comet's tail" of retained notes
4. **The melody emerges** from the temporal synthesis, not from individual notes

The melody exists only through the temporal synthesis that consciousness performs, retaining past notes while receiving present ones.

## Example 2: Reading and Understanding - Linguistic Temporal Objects

Let's model how consciousness constitutes meaning through temporal synthesis when reading.

### Words as Immanent Objects

```haskell
data Word = Word
    { text :: Text
    , meaning :: Text
    , grammaticalRole :: Text  -- "subject", "verb", "object", etc.
    } deriving (Show, Eq, Typeable)

instance Immanent IO Word where
    noesis word = pure $ Just word
    
    noema word = Just $ Noema
        { sense = "word-as-understood"
        , objects = [wordObject]
        , determinations =
            [ Determination ("meaning:" <> word.meaning) 0.9
            , Determination ("role:" <> word.grammaticalRole) 0.7
            ]
        }
    where
        wordObject = NoematicObject $ hash word.text

-- A sentence to read
sentence :: [Word]
sentence = 
    [ Word "The" "definite-article" "determiner"
    , Word "cat" "feline-animal" "subject"
    , Word "sat" "past-sitting" "verb"
    , Word "on" "spatial-relation" "preposition"
    , Word "the" "definite-article" "determiner"
    , Word "mat" "floor-covering" "object"
    ]

-- Read the sentence through time-consciousness
readSentence :: [Word] -> Eff '[ImmanentTime, Intentionality, IOE] Text
readSentence words = do
    putStrLn "Reading sentence through time-consciousness..."
    
    -- Present each word to consciousness
    forM_ words $ \word -> do
        presentOne word
        progressPresent
    
    -- The sentence meaning emerges from temporal synthesis
    retention <- getRetention
    putStrLn $ "Sentence synthesized through " <> show (V.length retention) <> " temporal phases"
    
    -- Extract the emergent meaning (simplified)
    pure "The cat sat on the mat (meaning constituted through temporal synthesis)"
```

## Example 3: Memory and Retention - The Temporal Horizon

Let's explore how memory works in time-consciousness, distinguishing between primary retention and recollective memory.

### Modeling Different Types of Memory

```haskell
data MemoryContent = MemoryContent
    { content :: Text
    , memoryType :: MemoryType
    , vividness :: Float  -- 0.0 to 1.0
    } deriving (Show, Eq, Typeable)

data MemoryType 
    = PrimaryRetention    -- Just-past, still in temporal horizon
    | RecollectiveMemory  -- Deliberately recalled past
    | Anticipation        -- Future-directed memory
    deriving (Show, Eq)

instance Immanent IO MemoryContent where
    noesis memory = case memory.memoryType of
        PrimaryRetention -> pure $ Just memory  -- Always accessible
        RecollectiveMemory -> 
            -- Recollection might fail or transform the memory
            if memory.vividness > 0.3 
                then pure $ Just memory
                else pure Nothing
        Anticipation -> pure $ Just memory
    
    noema memory = Just $ Noema
        { sense = case memory.memoryType of
            PrimaryRetention -> "just-past-as-retained"
            RecollectiveMemory -> "past-as-recollected"
            Anticipation -> "future-as-anticipated"
        , objects = [memoryObject]
        , determinations = 
            [ Determination memory.content memory.vividness ]
        }
    where
        memoryObject = NoematicObject $ hash (memory.content, memory.memoryType)

-- Demonstrate memory and retention
memoryExample :: Eff '[ImmanentTime, Intentionality, IOE] ()
memoryExample = do
    putStrLn "Exploring memory and retention..."
    
    -- Present a series of experiences
    let experiences = 
            [ MemoryContent "I see a red rose" PrimaryRetention 1.0
            , MemoryContent "I smell its fragrance" PrimaryRetention 1.0
            , MemoryContent "I remember my grandmother's garden" RecollectiveMemory 0.7
            , MemoryContent "I anticipate picking the rose" Anticipation 0.6
            ]
    
    forM_ experiences $ \exp -> do
        putStrLn $ "Experiencing: " <> exp.content
        presentOne exp
        progressPresent
        
        -- Show how retention builds up
        retentionLength <- getRetentionLength
        putStrLn $ "Retention horizon: " <> show retentionLength <> " phases"
    
    -- Demonstrate bounded retention
    capacity <- getRetentionCapacity
    putStrLn $ "Retention capacity: " <> show capacity
    
    -- Show final retention state
    retention <- getRetention
    putStrLn $ "Final retention contains " <> show (V.length retention) <> " phases"
```

## Example 4: Intentional Acts and Object Constitution

Let's explore how consciousness constitutes objects through intentional acts.

### Different Types of Intentional Acts

```haskell
data IntentionalAct = IntentionalAct
    { actType :: ActType
    , target :: Text
    , quality :: ActQuality
    } deriving (Show, Eq, Typeable)

data ActType 
    = Perceiving | Remembering | Imagining | Judging | Willing
    deriving (Show, Eq)

data ActQuality
    = Positing      -- Asserting existence
    | Neutralizing  -- Bracketing existence
    | Doubting      -- Questioning existence
    deriving (Show, Eq)

instance Immanent IO IntentionalAct where
    noesis act = case act.actType of
        Perceiving -> pure $ Just act  -- Direct givenness
        Remembering -> pure $ Just act  -- Retained givenness
        Imagining -> pure $ Just act   -- Free variation
        Judging -> pure $ Just act     -- Predicative synthesis
        Willing -> pure $ Just act     -- Practical intentionality
    
    noema act = Just $ Noema
        { sense = T.pack (show act.actType) <> "-of-" <> act.target
        , objects = [targetObject]
        , determinations = 
            [ Determination ("act-type:" <> T.pack (show act.actType)) 1.0
            , Determination ("quality:" <> T.pack (show act.quality)) 0.8
            ]
        }
    where
        targetObject = NoematicObject $ hash act.target

-- Demonstrate object constitution through intentional acts
intentionalityExample :: Eff '[ImmanentTime, Intentionality, IOE] ()
intentionalityExample = do
    putStrLn "Exploring intentional acts and object constitution..."
    
    -- Create a noematic object
    houseObject <- newObject
    putStrLn $ "Constituted object: " <> show houseObject
    
    -- Different intentional acts directed toward the same object
    let acts = 
            [ IntentionalAct Perceiving "house" Positing
            , IntentionalAct Remembering "house" Positing  
            , IntentionalAct Imagining "house" Neutralizing
            , IntentionalAct Judging "house" Positing
            ]
    
    forM_ acts $ \act -> do
        putStrLn $ "Performing act: " <> show act
        presentOne act
        
        -- Show how the same object can be intended differently
        present <- getPresent
        putStrLn $ "Current synthesis: " <> show present
        
        progressPresent
```

## Example 5: Temporal Objects and Duration

Let's model how consciousness constitutes objects with temporal extension.

### A Temporal Object: A Conversation

```haskell
data ConversationTurn = ConversationTurn
    { speaker :: Text
    , utterance :: Text
    , timestamp :: Float
    , emotionalTone :: Text
    } deriving (Show, Eq, Typeable)

instance Immanent IO ConversationTurn where
    noesis turn = pure $ Just turn
    
    noema turn = Just $ Noema
        { sense = "conversational-turn-as-heard"
        , objects = [turnObject]
        , determinations =
            [ Determination ("speaker:" <> turn.speaker) 1.0
            , Determination ("content:" <> turn.utterance) 0.9
            , Determination ("tone:" <> turn.emotionalTone) 0.6
            ]
        }
    where
        turnObject = NoematicObject $ hash (turn.speaker, turn.utterance)

-- A conversation as a temporal object
conversation :: [ConversationTurn]
conversation = 
    [ ConversationTurn "Alice" "Hello, how are you?" 0.0 "friendly"
    , ConversationTurn "Bob" "I'm doing well, thanks!" 2.5 "cheerful"
    , ConversationTurn "Alice" "That's great to hear." 5.0 "warm"
    , ConversationTurn "Bob" "How about you?" 7.0 "interested"
    ]

-- Experience the conversation as a temporal object
conversationExample :: Eff '[ImmanentTime, Intentionality, IOE] ()
conversationExample = do
    putStrLn "Experiencing conversation as temporal object..."
    
    forM_ conversation $ \turn -> do
        putStrLn $ turn.speaker <> ": " <> turn.utterance
        presentOne turn
        
        -- Show how the conversation builds up through retention
        retention <- getRetention
        putStrLn $ "Conversation context: " <> show (V.length retention) <> " turns retained"
        
        progressPresent
    
    putStrLn "Conversation complete - meaning emerges from temporal synthesis"
```

## Running the Examples

To run all examples:

```haskell
main :: IO ()
main = do
    putStrLn "=== Heco Subjectivity Tutorial ==="
    putStrLn ""
    
    putStrLn "1. Melody Example:"
    runMelodyExample
    putStrLn ""
    
    putStrLn "2. Reading Example:"
    runEff $ runRingBufferImmanentTimeEx (RingBufferOps 10) $
        runStandardIntentionality $ do
            meaning <- readSentence sentence
            liftIO $ putStrLn $ "Emergent meaning: " <> T.unpack meaning
    putStrLn ""
    
    putStrLn "3. Memory Example:"
    runEff $ runRingBufferImmanentTimeEx (RingBufferOps 5) $
        runStandardIntentionality memoryExample
    putStrLn ""
    
    putStrLn "4. Intentionality Example:"
    runEff $ runRingBufferImmanentTimeEx (RingBufferOps 5) $
        runStandardIntentionality intentionalityExample
    putStrLn ""
    
    putStrLn "5. Conversation Example:"
    runEff $ runRingBufferImmanentTimeEx (RingBufferOps 8) $
        runStandardIntentionality conversationExample
```

## Key Insights

Through these examples, you should understand:

1. **Temporal Synthesis**: Objects like melodies and sentences emerge through temporal synthesis, not from individual components

2. **Retention vs. Memory**: Primary retention (the "comet's tail") is different from recollective memory

3. **Intentional Structure**: Consciousness is always directed toward objects through specific intentional acts

4. **Object Constitution**: Objects are constituted through consciousness, not simply "given"

5. **Bounded Retention**: Consciousness has finite retention capacity, modeling realistic temporal limits

6. **Temporal Objects**: Some objects (melodies, conversations) exist only through temporal synthesis

## Further Exploration

Try modifying these examples to explore:

- Different retention capacities and their effects
- More complex noematic structures
- Interactions between different types of intentional acts
- Temporal objects with different temporal structures
- Error conditions and recovery in temporal synthesis

This computational approach to phenomenology opens new possibilities for understanding consciousness, temporal experience, and the constitution of meaning through time.
