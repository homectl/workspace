module Graphics.GPipe.Engine.TimeIt
  ( timeIt
  , timeItInPlace
  , Status (..)
  , Info (..)
  ) where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Time.Clock        as Time
import           Graphics.GPipe         (Buffer (..), Texture2D)
import           System.IO              (hFlush, stdout)


timeItInPlace :: (Info a, MonadIO m) => String -> m a -> m a
timeItInPlace text m = do
    liftIO $ putStr ("[" ++ show Running ++ "] " ++ text) >> hFlush stdout
    s <- liftIO Time.getCurrentTime
    (r, (status, info)) <- getInfo <$> m
    liftIO $ putStr (" " ++ info ++ replicate (40 - length info - length text) ' ') >> hFlush stdout
    e <- liftIO Time.getCurrentTime
    let elapsed = Time.nominalDiffTimeToSeconds $ Time.diffUTCTime e s
        ops = if elapsed > 0 then 1 / elapsed else -1
    liftIO $ putStr $ show elapsed ++ " (" ++ show ops ++ "/sec)             "
    liftIO $ putStr ("\r[" ++ show status ++ "\r")
    return r


timeIt :: (Info a, MonadIO m) => String -> m a -> m a
timeIt text m = do
    r <- timeItInPlace text m
    liftIO $ putStr "\n"
    return r


data Status
    = Fail
    | Done
    | Running

instance Show Status where
    show Fail    = "\027[1;31mFAIL\027[0m"
    show Done    = "\027[1;32mDONE\027[0m"
    show Running = "\027[1;33m....\027[0m"

class Info a where
    getInfo :: a -> (a, (Status, String))

instance Info (Maybe a) where
    getInfo r@Nothing = (r, (Done, ""))
    getInfo r@Just{}  = (r, (Done, "OK"))

instance Info [a] where
    getInfo r = (r, (Done, "(" ++ show (length r) ++ ")"))

instance Info b => Info (Either a b) where
    getInfo r@Left{}     = (r, (Fail, ""))
    getInfo r@(Right ok) = (r, snd $ getInfo ok)

instance Info (Texture2D a b) where
    getInfo r = (r, (Done, ""))

instance Info (a -> b) where
    getInfo r = (r, (Done, ""))

instance Info (Buffer os a) where
    getInfo r = (r, (Done, "(" ++ show (bufferLength r) ++ ")"))
