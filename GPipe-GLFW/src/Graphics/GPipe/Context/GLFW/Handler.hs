{-# LANGUAGE TypeFamilies #-}
-- | Internal module defining handler and its ContextHandler instance as well as some methods
module Graphics.GPipe.Context.GLFW.Handler where

-- stdlib
import           Control.Concurrent                   (MVar, ThreadId,
                                                       modifyMVar_, myThreadId,
                                                       newMVar, withMVar)
import           Control.Concurrent.Async             (withAsync)
import           Control.Concurrent.STM               (atomically)
import           Control.Concurrent.STM.TVar          (TVar, modifyTVar,
                                                       newTVarIO, readTVarIO,
                                                       writeTVar)
import           Control.Exception                    (Exception, throwIO)
import           Control.Monad                        (forM, forM_, unless,
                                                       void, when)
import           Control.Monad.IO.Class               (MonadIO, liftIO)
import           Data.List                            (delete, partition)
import           Data.Maybe                           (fromMaybe)
import           Text.Printf                          (printf)
-- thirdparty
import qualified Graphics.GPipe                       as GPipe (ContextHandler (..),
                                                                ContextT,
                                                                Window,
                                                                WindowBits,
                                                                withContextWindow)
import qualified Graphics.UI.GLFW                     as GLFW (Error, Window)
-- local
import qualified Graphics.GPipe.Context.GLFW.Calls    as Call
import qualified Graphics.GPipe.Context.GLFW.Format   as Format
import qualified Graphics.GPipe.Context.GLFW.Logger   as Log
import qualified Graphics.GPipe.Context.GLFW.RPC      as RPC
import           Graphics.GPipe.Context.GLFW.Resource (defaultWindowConfig)
import qualified Graphics.GPipe.Context.GLFW.Resource as Resource

-- | Internal handle for a GPipe-created GLFW window/context
newtype Context = Context
    { contextRaw :: GLFW.Window
--  , contextComm :: RPC.Handle
--  , contextAsync :: Async ()
    }
-- | Closeable internal handle for 'Context'.
type MMContext = MVar (Maybe Context)

-- | Opaque handle representing the initialized GLFW library.
--
-- To get started quickly try 'defaultHandleConfig' and 'defaultWindowConfig'.
--
-- @
--      import Graphics.GPipe
--      import qualified Graphics.GPipe.Context.GLFW as GLFW
--
--      runContextT GLFW.defaultHandleConfig $ do
--          win <- newWindow (WindowFormatColorDepth RGB8 Depth16) (GLFW.defaultWindowConfig "OpenGL Graphics")
--          -- Do GPipe things here
-- @
data Handle = Handle
    { handleTid         :: ThreadId
    , handleComm        :: RPC.Handle
    , handleRaw         :: GLFW.Window
    , handleCtxs        :: TVar [MMContext]
    , handleEventPolicy :: Maybe EventPolicy
    , handleLogger      :: Log.Logger
    }

-- | Opaque handle representing a, possibly closed, internal 'Context'. You'll
-- typically deal with GPipe's @Window@ instead of this one.
newtype GLFWWindow = WWindow (MMContext, Handle)

-- | Run the action with the context /if the context is still open/.
withContext :: String -> MMContext -> (Context -> IO a) -> IO (Maybe a)
withContext callerTag mmContext action = withMVar mmContext go
    where
        go Nothing = printf "WARNING %s: GPipe-GLFW context already closed" callerTag >> return Nothing
        go (Just context) = pure <$> action context

-- | Template for "Run the action with XYZ /if the gpipe window still exists and ABC/."
unwrappingGPipeWindow :: MonadIO m
    => (String -> action -> Handle -> MMContext -> IO (Maybe a)) -- ^ Specialize use of unwrappingGPipeWindow
    -> String -> GPipe.Window os c ds -> action -> GPipe.ContextT Handle os m (Maybe a)
unwrappingGPipeWindow specialize callerTag wid action = GPipe.withContextWindow wid go
    where
        go Nothing = printf "WARNING %s: GPipe had no such window" callerTag >> return Nothing
        go (Just (WWindow (mmContext, handle))) = specialize callerTag action handle mmContext

-- | Run the action with the context __handle__ /if the gpipe window still exists/.
withHandleFromGPipe :: MonadIO m => String -> GPipe.Window os c ds -> (Handle -> IO a) -> GPipe.ContextT Handle os m (Maybe a)
withHandleFromGPipe = unwrappingGPipeWindow $ \_callerTag action handle _mmContext ->
    Just <$> action handle

---- | Run the action with the __context__ /if the gpipe window still exists and corresponding context is still open/.
withContextFromGPipe :: MonadIO m => String -> GPipe.Window os c ds -> (Context -> IO a) -> GPipe.ContextT Handle os m (Maybe a)
withContextFromGPipe = unwrappingGPipeWindow $ \callerTag action _handle mmContext ->
    withContext callerTag mmContext action

withBothFromGPipe :: MonadIO m => String -> GPipe.Window os c ds -> (Handle -> Context -> IO a) -> GPipe.ContextT Handle os m (Maybe a)
withBothFromGPipe = unwrappingGPipeWindow $ \callerTag action handle mmContext ->
    withContext callerTag mmContext (action handle)

-- | Route an effect to the main thread.
effectMain :: Handle -> Call.EffectMain
effectMain handle = RPC.sendEffect (handleComm handle)

-- | Route an action with a result to the main thread.
onMain :: Handle -> Call.OnMain a
onMain handle = RPC.fetchResult (handleComm handle)

-- | Default GLFW handle configuration.
--
-- * Print any errors that GLFW emits.
-- * Automatically process GLFW events after every buffer swap.
-- * Log only context handling activity which represents undesired conditions.
defaultHandleConfig :: GPipe.ContextHandlerParameters Handle
defaultHandleConfig = HandleConfig
    { configErrorCallback = printf "%s: %s\n" . show
    , configEventPolicy = pure Poll
    , configLogger = Log.Logger
        { Log.loggerLevel = Log.WARNING
        , Log.loggerSink = Log.stderrSink
        }
    }

instance GPipe.ContextHandler Handle where

    -- | Configuration for the GLFW handle.
    data ContextHandlerParameters Handle = HandleConfig
        { -- | Specify a callback to handle errors emitted by GLFW.
          configErrorCallback :: GLFW.Error -> String -> IO ()
          -- | Specify the 'EventPolicy' to use for automatic GLFW event
          -- processing. Set to 'Nothing' to disable automatic event processing
          -- (you'll need to call 'mainloop' or 'mainstep').
        , configEventPolicy :: Maybe EventPolicy
          -- | Configuration for emitting messages.
        , configLogger :: Log.Logger
        }

    type ContextWindow Handle = GLFWWindow
    type WindowParameters Handle = Resource.WindowConfig

    -- Thread assumption: any thread
    --
    -- Create a context which shares objects with the contexts created by this
    -- handle, if any.
    createContext handle settings = do
        window <- createWindow (handleLogger handle) (Just $ handleRaw handle) settings
        mmContext <- newMVar . pure $ Context window
        atomically $ modifyTVar (handleCtxs handle) (mmContext :)
        return $ WWindow (mmContext, handle)

    -- Threading assumption: any thread
    --
    -- Do work with the specified context by making it current. If no context
    -- is specified, then any context being current is sufficient.
    --
    -- XXX: If there's a lot of context swapping, change this to RPC to a
    -- context-private thread running a mainloop.
    contextDoAsync handle Nothing action = RPC.sendEffect (handleComm handle) $ do
        -- (on main thread) Make the ancestor current if nothing else already is
        -- FIXME: these two bodies could be combined, perhaps.. the RPC is only necessary if the current thread lacks a context
        ccHuh <- Call.getCurrentContext
        maybe (Call.makeContextCurrent (handleLogger handle) "contextDoAsync required some context" . pure $ handleRaw handle)
            (const $ return ())
            ccHuh
        action
    contextDoAsync _ (Just (WWindow (mmContext, handle))) action =
        void $ withContext "contextDoAsync" mmContext $ \context -> do
            Call.makeContextCurrent (handleLogger handle) "contextDoAsync required a specific context" . pure . contextRaw $ context
            action

    -- Threading assumption: main thread
    --
    -- Swap buffers for the specified context. If an event policy is set,
    -- process events.
    contextSwap _ (WWindow (mmContext, handle)) = do
        void $ withContext "contextSwap" mmContext $ Call.swapBuffers . contextRaw
        mapM_ (mainstepInternal handle) $ handleEventPolicy handle

    -- Threading assumption: same thread as contextCreate for the given context
    --
    -- Fetch framebuffer size for the specified context by RPCing the main thread.
    contextFrameBufferSize _ (WWindow (mmContext, handle)) = do
        result <- withContext "contextFrameBufferSize" mmContext $ \context -> do
            Call.getFramebufferSize (onMain handle) $ contextRaw context
        maybe failure return result
        where
            failure = do
                Call.say (handleLogger handle) Log.ERROR $ printf "contextFrameBufferSize could not access context"
                return (0, 0)

    -- Threading assumption: same thread as contextCreate for the given context
    --
    -- Destroy the given context by making it current on the main thread and
    -- then destroying it there.
    --
    -- Note: See the restrictions for Call.destroyWindow
    contextDelete _ (WWindow (mmContext, handle)) = do
        -- close the context mvar
        modifyMVar_ mmContext $ \mContext -> do
            Call.say (handleLogger handle) Log.INFO $ printf "contextDelete of %s" (show $ contextRaw <$> mContext)
            forM_ mContext $ \context -> RPC.sendEffect (handleComm handle) $ do
                Call.makeContextCurrent (handleLogger handle) "contextDelete" . pure . contextRaw $ context
                Call.destroyWindow id (contextRaw context) -- id RPC because this is in a mainthread RPC
            return Nothing
        -- remove the context from the handle
        atomically $ modifyTVar (handleCtxs handle) (delete mmContext)

    -- Threading assumption: main thread
    contextHandlerCreate config = do
        Call.say (configLogger config) Log.DEBUG "contextHandlerCreate"
        -- make handle resources
        tid <- myThreadId
        comm <- RPC.newBound
        ctxs <- newTVarIO []
        -- initialize glfw
        Call.setErrorCallback id . pure $ configErrorCallback config -- id RPC because contextHandlerCreate is called only on mainthread
        ok <- Call.init id -- id RPC because contextHandlerCreate is called only on mainthread
        unless ok $ throwIO InitException
        -- wrap up handle
        ancestor <- createWindow (configLogger config) Nothing Nothing
        return $ Handle
            { handleTid = tid
            , handleComm = comm
            , handleRaw = ancestor
            , handleCtxs = ctxs
            , handleEventPolicy = configEventPolicy config
            , handleLogger = configLogger config
            }

    -- Threading: main thread
    contextHandlerDelete handle = do
        Call.say (handleLogger handle) Log.DEBUG "contextHandlerDelete"
        ctxs <- readTVarIO $ handleCtxs handle
        forM_ ctxs $ \mmContext -> GPipe.contextDelete handle (WWindow (mmContext, handle))
        atomically $ writeTVar (handleCtxs handle) []
        -- all resources are released
        Call.terminate id -- id RPC because contextHandlerDelete is called only on mainthread
        Call.setErrorCallback id Nothing -- id RPC because contextHandlerDelete is called only on mainthread

-- Create a raw GLFW window for use by contextHandlerCreate & createContext
createWindow :: Log.Logger -> Maybe GLFW.Window -> Maybe (GPipe.WindowBits, Resource.WindowConfig) -> IO GLFW.Window
createWindow logger parentHuh settings = do
    unless (null disallowedHints) $
        throwIO $ Format.UnsafeWindowHintsException disallowedHints
    -- make a context
    windowHuh <- Call.createWindow id width height title monitor hints parentHuh -- id RPC because contextHandlerCreate & createContext are called only on mainthread
    Call.say logger Log.DEBUG $ printf "made context %s -> parent %s" (show windowHuh) (show parentHuh)
    window <- maybe exc return windowHuh
    -- set up context
    forM_ intervalHuh $ \interval -> do
        Call.makeContextCurrent logger "apply vsync setting" $ pure window
        Call.swapInterval interval
    -- done
    return window
    where
        config = maybe (defaultWindowConfig "") snd settings
        Resource.WindowConfig {Resource.configWidth=width, Resource.configHeight=height} = config
        Resource.WindowConfig _ _ title monitor _ intervalHuh = config
        (userHints, disallowedHints) = partition Format.allowedHint $ Resource.configHints config
        hints = userHints ++ Format.bitsToHints (fst <$> settings) ++ Format.unconditionalHints
        exc = throwIO . CreateSharedWindowException . show $ config {Resource.configHints = hints}

-- | Type to describe the waiting or polling style of event processing
-- supported by GLFW.
--
-- * Recommended reading: /Event Processing/ section of the GLFW /Input Guide/
-- at <http://www.glfw.org/docs/latest/input_guide.html#events>.
data EventPolicy
    = Poll
    | Wait
    | WaitTimeout Double
    deriving
    ( Show
    )

-- | Process GLFW and GPipe events according to the given 'EventPolicy'.
--
-- __Use case:__ Call 'mainstep' as part of a custom engine loop in multithreaded
-- applications which do GPipe rendering off of the main thread. Use 'mainloop'
-- for less complex applications.
--
-- * Must be called on the main thread.
-- * Can be called with /any/ window you've created and not yet deleted.
-- * If GPipe can't find the window you passed in, returns 'Nothing'.
mainstep :: MonadIO m
    => GPipe.Window os c ds
    -> EventPolicy -- ^ 'Poll' will process events and return immediately while 'Wait' will sleep until events are received.
    -> GPipe.ContextT Handle os m (Maybe ())
mainstep win eventPolicy = withHandleFromGPipe "mainstep" win $ liftIO . flip mainstepInternal eventPolicy

mainstepInternal :: Handle -> EventPolicy -> IO ()
mainstepInternal handle eventPolicy = do
    tid <- myThreadId
    when (tid /= handleTid handle) $
        throwIO $ MainstepOffMainException "mainstep must be called from main thread"
    case eventPolicy of
        Poll -> Call.pollEvents id -- id RPC because mainstepInternal is called only on mainthread
        Wait -> withAsync
                    -- Async sleeps on RPC chan, waking op main when RPC received
                    (RPC.awaitActions (handleComm handle) >> Call.postEmptyEvent)
                    -- Main sleeps on waitEvents
                    (const $ Call.waitEvents id) -- id RPC because mainstepInternal is called only on mainthread
        WaitTimeout timeout -> withAsync
                    -- Async sleeps on RPC chan, waking op main when RPC received
                    (RPC.awaitActions (handleComm handle) >> Call.postEmptyEvent)
                    -- Main sleeps on waitEventsTimeout
                    (const $ Call.waitEventsTimeout id timeout) -- id RPC because mainstepInternal is called only on mainthread
    RPC.processActions $ handleComm handle

-- | Process GLFW and GPipe events according to the given 'EventPolicy' in a
-- loop.
--
-- __Use case:__ Call 'mainloop' in multithreaded applications which do GPipe
-- rendering off of the main thread, but which do not otherwise need additional
-- control over the main thread. For less complex applications use automatic
-- event processing configured via 'HandleConfig'.
--
-- * Must be called on the main thread.
-- * The loop will run until 'windowShouldClose' is true for the all 'Window's
-- created by the same 'ContextHandler', or all the 'Window's have been
-- deleted.
-- * To indicate a window should close use 'setWindowShouldClose' in "Graphics.GPipe.Context.GLFW.Wrapped".
mainloop :: MonadIO m
    => GPipe.Window os c ds
    -> EventPolicy -- ^ A 'Poll' loop runs continuously while a 'Wait' loop sleeps until events or user input occur.
    -> GPipe.ContextT Handle os m (Maybe ())
mainloop win eventPolicy = withHandleFromGPipe "mainloop" win $ liftIO . flip mainloopInternal eventPolicy

mainloopInternal :: Handle -> EventPolicy -> IO ()
mainloopInternal handle eventPolicy = do
    mainstepInternal handle eventPolicy
    ctxs <- readTVarIO $ handleCtxs handle
    allShouldClose <- and <$> forM ctxs oneShouldClose
    unless allShouldClose $
        mainloopInternal handle eventPolicy
    where
        oneShouldClose mmContext = do
            shouldCloseHuh <- withContext "oneShouldClose" mmContext $ Call.windowShouldClose . contextRaw
            return $ fromMaybe True shouldCloseHuh

-- | IO exception thrown when GLFW library initialization fails.
data InitException = InitException
    deriving Show
instance Exception InitException

-- | IO Exception thrown when GLFW window creation fails.
data CreateWindowException
    = CreateWindowException String
    | CreateSharedWindowException String
    deriving Show
instance Exception CreateWindowException

-- | IO Exception thrown when application code calls a GPipe-GLFW incorrectly
-- (eg. on the wrong thread).
newtype UsageException
    = MainstepOffMainException String
    deriving Show
instance Exception UsageException
