module Graphics.GPipe.Context.GLFW.RPC where

-- stdlib
import           Control.Concurrent            (ThreadId, myThreadId)
import           Control.Concurrent.MVar       (newEmptyMVar, putMVar, takeMVar)
import           Control.Concurrent.STM        (STM, atomically)
import           Control.Concurrent.STM.TQueue (TQueue, newTQueue, peekTQueue,
                                                tryReadTQueue, writeTQueue)
import           Data.Sequence                 (Seq, empty, (|>))
-- local
--import qualified Graphics.GPipe.Context.GLFW.Calls as Call

data Handle = Handle ThreadId (TQueue RPC)
    deriving
    ( Eq
    )

-- TODO: change RPC to a chan of `IO ()` and collapse `runActions`
data RPC
    = Execute (IO ())
    | Noop

-- | Create an RPC handle bound to the current thread. Actions sent from the
-- bound thread will just be run w/o doing an RPC.
newBound :: IO Handle
newBound = do
    tid <- myThreadId
    comm <- atomically $ newTQueue
    return $ Handle tid comm

-- XXX: consider pushing thread-check to all callsites of sendEffect, fetchResult
-- TODO: dry-up thread id check
sendEffect :: Handle -> IO () -> IO ()
sendEffect (Handle boundTid comm) action = do
    tid <- myThreadId
    if boundTid == tid
        then action
        else atomically $ writeTQueue comm (Execute action)

fetchResult :: Handle -> IO a -> IO a
fetchResult (Handle boundTid comm) action = do
    tid <- myThreadId
    if boundTid == tid
        then action
        else do
            reply <- newEmptyMVar
            -- XXX: Make sure the value put in the MVar is evaluated first
            atomically$ writeTQueue comm (Execute $ action >>= putMVar reply)
            takeMVar reply

drainComm :: TQueue a -> STM (Seq a)
drainComm queue = go empty
    where
        go rpcs = do
            result <- tryReadTQueue queue
            case result of
                Just rpc -> go $ rpcs |> rpc
                Nothing  -> return rpcs

runActions :: Foldable t => t RPC -> IO ()
runActions = mapM_ go
    where
        go Noop             = print "noop"
        go (Execute action) = action

awaitActions :: Handle -> IO RPC
awaitActions (Handle _ comm) = atomically . peekTQueue $ comm

processActions :: Handle -> IO ()
processActions (Handle _ comm) = (atomically . drainComm $ comm) >>= runActions
