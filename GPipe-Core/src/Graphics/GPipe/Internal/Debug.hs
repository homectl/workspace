module Graphics.GPipe.Internal.Debug where

import           Control.Monad      (when)
import           Data.List          (intercalate)
import           Data.Maybe         (fromMaybe)
import qualified Debug.Trace        as Trace
import           Graphics.GL.Core45
import           Graphics.GL.Types  (GLenum)

traceIt :: Show a => String -> a -> a
traceIt t a = Trace.trace (t ++ " = " ++ show a) a

traceList :: Show a => String -> [a] -> [a]
traceList t as = Trace.trace (t ++ " = [\n\t" ++ intercalate "\n\t" (map show as) ++ "\n]") as

checkGlError :: String -> IO ()
checkGlError title = do
    e <- glGetError
    when (e /= GL_NO_ERROR) $ do
        error $ "[" ++ title ++ "] GL error -> " ++ getErrorMessage e

getErrorMessage :: GLenum -> String
getErrorMessage errorCode = fromMaybe ("Unknown error code " ++ show errorCode) $ lookup errorCode
    [ (GL_NO_ERROR, "GL_NO_ERROR")
    , (GL_INVALID_ENUM, "GL_INVALID_ENUM")
    , (GL_INVALID_VALUE, "GL_INVALID_VALUE")
    , (GL_INVALID_OPERATION, "GL_INVALID_OPERATION")
    , (GL_STACK_OVERFLOW, "GL_STACK_OVERFLOW")
    , (GL_STACK_UNDERFLOW, "GL_STACK_UNDERFLOW")
    , (GL_OUT_OF_MEMORY, "GL_OUT_OF_MEMORY")
    , (GL_INVALID_FRAMEBUFFER_OPERATION, "GL_INVALID_FRAMEBUFFER_OPERATION")
    , (GL_CONTEXT_LOST, "GL_CONTEXT_LOST")
    ]
