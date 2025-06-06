module Heco.Data.Task where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (NominalDiffTime, UTCTime, TimeOfDay)
import Data.Hashable (Hashable)
import Pattern.Cast (Cast(..))

newtype TaskId = TaskId Int
    deriving (Show, Eq, Ord, Enum, Bounded)
    deriving Num via Int
    deriving newtype Hashable
    
instance Cast TaskId Int where
    cast (TaskId id) = id

data TaskTrigger
    = OneShotTrigger
    | PeriodicTrigger NominalDiffTime
    | DelayedTrigger NominalDiffTime
    | UTCTimeTrigger UTCTime
    | TimeOfDayTrigger TimeOfDay
    | RestartOnException Int TaskTrigger
    deriving (Eq, Show)

data TaskStage
    = InitialStage
    | RunningStage
    | ScheduledStage
    | StoppedStage
    deriving (Eq, Show)

data Task m = Task
    { name :: Text
    , description :: Maybe Text
    , trigger :: TaskTrigger
    , procedure :: m () }

instance Show (Task m) where
    show t = "Task <" ++ T.unpack t.name ++ ">"

withTaskDescription :: Text -> Task m -> Task m
withTaskDescription desc t = t { description = Just desc }

withTrigger :: TaskTrigger -> Task m -> Task m
withTrigger trg t = t { trigger = trg }

oneShotTask :: Text -> m () -> Task m
oneShotTask name proc = Task
    { name = name
    , description = Nothing
    , procedure = proc
    , trigger = OneShotTrigger }

periodicTask :: Text -> NominalDiffTime -> m () -> Task m
periodicTask name duration proc = Task
    { name = name
    , description = Nothing
    , procedure = proc
    , trigger = PeriodicTrigger duration }

delayedTask :: Text -> NominalDiffTime -> m () -> Task m
delayedTask name delay proc = Task
    { name = name
    , description = Nothing
    , procedure = proc
    , trigger = DelayedTrigger delay }

utcTimeTask :: Text -> UTCTime -> m () -> Task m
utcTimeTask name time proc = Task
    { name = name
    , description = Nothing
    , procedure = proc
    , trigger = UTCTimeTrigger time }

timeOfDayTask :: Text -> UTCTime -> m () -> Task m
timeOfDayTask name time proc = Task
    { name = name
    , description = Nothing
    , procedure = proc
    , trigger = UTCTimeTrigger time }

restartOnException :: Int -> Task m -> Task m
restartOnException retry task = task
    { trigger = RestartOnException retry task.trigger }