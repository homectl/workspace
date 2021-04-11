{-# LANGUAGE NamedFieldPuns #-}
module Graphics.GPipe.Context.GLFW.Logger where

import           System.IO   (hPutStrLn, stderr)
import           Text.Printf (printf)

-- | Levels of severity for log messages.
data LogLevel = DEBUG | INFO | WARNING | ERROR
    deriving (Show, Eq, Ord, Enum, Bounded)

-- | An arbitrary sink into which log messages may be passed.
newtype LogSink = LogSink { runSink :: String -> IO () }

-- | Very simple logger handle.
data Logger = Logger
    { loggerLevel :: LogLevel
    , loggerSink  :: LogSink
    }

-- | Emit a message to the sink specified by 'Logger' if the provided
-- 'Loglevel' is more severe than the configured one.
emitLog :: Logger -> LogLevel -> String -> IO ()
emitLog Logger{loggerLevel, loggerSink} messageLevel message
    | loggerLevel <= messageLevel = runSink loggerSink $ printf "%s %s" (show messageLevel) message
    | otherwise = return ()

-- | A 'LogSink' which writes to the @stderr@ stream.
stderrSink :: LogSink
stderrSink = LogSink $ hPutStrLn stderr
