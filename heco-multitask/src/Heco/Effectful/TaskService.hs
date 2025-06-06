module Heco.Effectful.TaskService where

import Heco.Data.Task
    ( TaskId,
      TaskTrigger(..),
      TaskStage(..),
      Task(..) )
import Heco.Data.TaskError (TaskError(TaskNotFoundError))

import Effectful
    ( type (:>),
      Effect,
      Eff,
      Limit(Unlimited),
      Persistence(Persistent),
      UnliftStrategy(ConcUnlift), IOE, MonadIO (liftIO) )
import Effectful.Dispatch.Dynamic (HasCallStack, reinterpret, localUnlift, LocalEnv)
import Effectful.TH (makeEffect)
import Effectful.Error.Dynamic (Error, runError, CallStack, throwError)
import Effectful.Concurrent (Concurrent, forkIO, threadDelay, killThread, myThreadId)
import Effectful.Concurrent.MVar (MVar, newMVar, modifyMVar, readMVar)
import Effectful.State.Static.Shared (evalState, get, state, State, modify, gets, stateM)
import Effectful.Exception (catch, SomeException)

import Data.Text (Text)
import Data.Text qualified as T
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet (HashSet)
import Data.HashSet qualified as HashSet
import Data.Tuple.Extra (dupe)
import Data.Time
    ( nominalDiffTimeToSeconds,
      diffUTCTime,
      getCurrentTime,
      diffLocalTime,
      getZonedTime,
      LocalTime(localDay, LocalTime, localTimeOfDay),
      ZonedTime(zonedTimeToLocalTime), UTCTime )
import Data.Int (Int64)

import Control.Concurrent (ThreadId)
import Control.Monad (forever, when, void, forM_)
import Control.Monad.Extra (whenJust)

data TaskInfo = TaskInfo
    { id :: TaskId
    , name :: Text
    , description :: Maybe Text
    , trigger :: TaskTrigger
    , parent :: Maybe TaskId }

data TaskStatus = TaskStatus
    { stage :: TaskStage
    , createTime :: UTCTime
    , lastRunTime :: Maybe UTCTime
    , lastCompleteTime :: Maybe UTCTime
    , children :: HashSet TaskId }
    deriving (Eq, Show)

data TaskService :: Effect where
    StartTask :: Task m -> TaskService m TaskId
    StartSubTask :: TaskId -> Task m -> TaskService m TaskId
    StopTask :: TaskId -> TaskService m ()
    KillTask :: TaskId -> TaskService m ()
    ListTasks :: TaskService m [TaskInfo]
    LookupTask :: TaskId -> TaskService m (Maybe TaskInfo)
    GetTaskStatus :: TaskId -> TaskService m TaskStatus

makeEffect ''TaskService

startTask_ :: TaskService :> es => Task (Eff es) -> Eff es ()
startTask_ task = void $ startTask task

startSubTask_ :: TaskService :> es => TaskId -> Task (Eff es) -> Eff es ()
startSubTask_ parentId task = void $ startSubTask parentId task

data TaskEntry = TaskEntry
    { threadId :: ThreadId
    , info :: TaskInfo
    , status :: MVar TaskStatus }

type TaskMap = HashMap TaskId TaskEntry

removeTask ::
    ( HasCallStack
    , State TaskMap :> es
    , Concurrent :> es )
    => TaskId -> Eff es (Maybe TaskEntry)
removeTask id = do
    entry <- stateM @TaskMap \map -> do
        let (entry, map') = HashMap.alterF (, Nothing) id map
        case entry of
            Just e
                | Just parentId <- e.info.parent
                , Just parentEntry <- HashMap.lookup parentId map' -> do
                modifyMVar parentEntry.status \s ->
                    pure (s { children = HashSet.delete id s.children }, ())
            _ -> pure ()
        pure (entry, map')
    whenJust entry killChildren
    pure entry

removeTask_ ::
    ( State TaskMap :> es
    , Concurrent :> es )
    => TaskId -> Eff es ()
removeTask_ id = void $ removeTask id

killChildren ::
    ( HasCallStack
    , State TaskMap :> es
    , Concurrent :> es )
    => TaskEntry -> Eff es ()
killChildren entry = do
    status <- readMVar entry.status
    forM_ status.children removeTask

getTask ::
    ( HasCallStack
    , State TaskMap :> es
    , Error TaskError :> es )
    => TaskId -> Eff es TaskEntry
getTask id = do
    res <- gets @TaskMap $ HashMap.lookup id
    case res of
        Nothing -> throwError $
            TaskNotFoundError $ "Task not found: " ++ show id
        Just entry -> pure entry

runTaskThread ::
    ( HasCallStack
    , IOE :> es
    , Concurrent :> es
    , State TaskMap :> es )
    => TaskInfo -> MVar TaskStatus -> Eff es () -> Eff es ThreadId
runTaskThread info status procedure = do
    forkIO $ scheduleTask info.trigger do
        modifyStatus \now s -> s
            { stage = RunningStage
            , lastRunTime = Just now }
        procedure `catch` handleError 1
        modifyStatus \now s -> s
            { stage = ScheduledStage
            , lastCompleteTime = Just now }
    where
        scheduleTask trigger action = case trigger of
            OneShotTrigger ->
                action >> removeTask_ info.id
            PeriodicTrigger delay ->
                forever $ suspend delay >> action
            DelayedTrigger delay ->
                suspend delay >> action >> removeTask_ info.id
            UTCTimeTrigger time -> do
                now <- liftIO getCurrentTime
                let delay = diffUTCTime time now
                suspend delay >> action >> removeTask_ info.id
            TimeOfDayTrigger timeOfDay -> forever do
                zonedTime <- liftIO getZonedTime
                let currLocalTime = zonedTimeToLocalTime zonedTime
                    currTimeOfDay = localTimeOfDay currLocalTime
                suspend $ diffLocalTime 
                    LocalTime
                        { localDay =
                            if currTimeOfDay > timeOfDay
                                then succ $ localDay currLocalTime
                                else localDay currLocalTime
                        , localTimeOfDay = timeOfDay }
                    currLocalTime
                action
            RestartOnException _ trg -> scheduleTask trg action
        
        modifyStatus mapper = do
            shouldRemove <- modifyMVar status \s -> case s.stage of
                StoppedStage -> do
                    myThreadId >>= killThread
                    pure (s, True)
                _ -> do
                    now <- liftIO getCurrentTime
                    pure (mapper now s, False)
            when shouldRemove $ removeTask_ info.id

        handleError retry (e :: SomeException) = do
            liftIO $ putStrLn $ "Exception caught when running task \'"
                ++ T.unpack info.name ++ "\': " ++ show e
            case info.trigger of
                RestartOnException maxRetry _ | maxRetry < retry -> do
                    liftIO $ putStrLn "Restarting..."
                    procedure `catch` handleError (retry + 1)
                _ -> pure ()

        suspend diffTime =
            suspendMicroseconds $ toMicroseconds diffTime

        suspendMicroseconds (microseconds :: Int64) =
            when (microseconds > 0) do
                let delay = min microseconds $ fromIntegral (maxBound :: Int)
                threadDelay $ fromIntegral delay
                suspendMicroseconds $ microseconds - delay

        toMicroseconds diffTime = 
            floor $ 1000 * 1000 * nominalDiffTimeToSeconds diffTime

createTask ::
    ( HasCallStack
    , IOE :> es
    , Concurrent :> es
    , State TaskMap :> es
    , State TaskId :> es )
    => LocalEnv localEs es
    -> UnliftStrategy
    -> Maybe TaskId
    -> Task (Eff localEs)
    -> Eff es TaskEntry
createTask env unliftStrategy parentId task = do
    id <- state $ dupe . (+1)
    now <- liftIO getCurrentTime
    status <- newMVar TaskStatus
        { stage = InitialStage
        , createTime = now
        , lastRunTime = Nothing
        , lastCompleteTime = Nothing
        , children = HashSet.empty }
    let info = TaskInfo
            { id = id
            , name = task.name
            , description = task.description
            , trigger = task.trigger
            , parent = parentId }
    localUnlift env unliftStrategy \unlift -> do
        tid <- runTaskThread info status $ unlift task.procedure
        pure TaskEntry
            { threadId = tid
            , info = info
            , status = status }

runStandardTaskService ::
    ( HasCallStack
    , IOE :> es
    , Concurrent :> es
    , Error TaskError :> es )
    => Eff (TaskService : es) a
    -> Eff es a
runStandardTaskService = reinterpret wrap \env -> \case
    StartTask task -> do
        entry <- createTask env unliftStrategy Nothing task
        let id = entry.info.id
        modify $ HashMap.insert id entry
        pure id

    StartSubTask parentId task -> do
        parentEntry :: TaskEntry <-
            gets (HashMap.lookup parentId)
            >>= maybe (throwError $ TaskNotFoundError $ "Parent task not found: " ++ show parentId) pure
        entry <- createTask env unliftStrategy (Just parentId) task
        let id = entry.info.id
        modify $ HashMap.insert id entry
        modifyMVar parentEntry.status \s ->
            pure (s { children = HashSet.insert id s.children }, ())
        pure id

    StopTask id -> do
        entry <- getTask id
        shouldRemove <- modifyMVar entry.status \s ->
            if s.stage == StoppedStage
                then killThread entry.threadId >> pure (s, True)
                else pure (s { stage = StoppedStage }, False)
        when shouldRemove $ removeTask_ entry.info.id

    KillTask id -> do
        entry <- getTask id
        removeTask_ id
        killThread entry.threadId
        
    ListTasks ->
        get @TaskMap >>= pure . HashMap.foldr' (\s l -> s.info:l) []

    LookupTask id -> do
        res <- gets @TaskMap (HashMap.lookup id)
        pure $ (\e -> e.info) <$> res 

    GetTaskStatus id -> do
        entry <- getTask id
        readMVar entry.status

    where
        wrap =
            evalState (HashMap.empty :: TaskMap)
            . evalState (-1 :: TaskId)
        unliftStrategy = ConcUnlift Persistent Unlimited

runStandardTaskServiceEx ::
    ( HasCallStack
    , IOE :> es
    , Concurrent :> es )
    => Eff (TaskService : Error TaskError : es) a
    -> Eff es (Either (CallStack, TaskError) a)
runStandardTaskServiceEx =
    runError . runStandardTaskService