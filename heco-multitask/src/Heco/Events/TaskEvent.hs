module Heco.Events.TaskEvent where
    
import Heco.Data.Task (TaskInfo)

data TaskEvent
    = OnTaskStarted TaskInfo
    | OnTaskTerminated TaskInfo