{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.SNMap
    ( SNMapReaderT
    , runSNMapReaderT
    , memoizeM
    , scopedM
    ) where

import           Control.Applicative              (Applicative)
import           Control.Monad.Exception          (MonadAsyncException,
                                                   MonadException)
import           Control.Monad.IO.Class           (MonadIO, liftIO)
import           Control.Monad.Trans.Class        (MonadTrans (..))
import           Control.Monad.Trans.State.Strict (StateT (StateT), evalStateT,
                                                   get, put)
import qualified Data.HashMap.Strict              as HT
import           System.Mem.StableName            (StableName, makeStableName)

{- A map (SN stands for stable name) to cache the results of computations
(m ends up being constrained to MonadIO m).
-}
newtype SNMap m a = SNMap { unSNMap :: HT.HashMap (StableName (m a)) a }

newSNMap :: SNMap m a
newSNMap = SNMap HT.empty

memoize :: MonadIO m
    => m (SNMap m a)        -- The "IO call" to retrieve our cache.
    -> (SNMap m a -> m ())  -- The "IO call" to store an updated cache.
    -> m a                  -- The "IO call" to execute and cache the result.
    -> m a                  -- The result being naturally also returned.
memoize getter putter m = do
    s <- liftIO $ makeStableName m
    x <- HT.lookup s . unSNMap <$> getter
    case x of
        Just a -> return a
        Nothing -> do
            a <- m
            -- Need to redo the getter action because of scopeM.
            getter >>= putter . SNMap . HT.insert s a . unSNMap
            return a

-- An (IO) action producing a 'b' value while caching 'a' values along the way.
newtype SNMapReaderT a m b = SNMapReaderT (StateT (SNMap (SNMapReaderT a m) a) m b)
    deriving (Functor, Applicative, Monad, MonadIO, MonadException, MonadAsyncException)

runSNMapReaderT :: MonadIO m => SNMapReaderT a m b -> m b
runSNMapReaderT (SNMapReaderT m) = do
    let h = newSNMap
    evalStateT m h

instance MonadTrans (SNMapReaderT a) where
    lift = SNMapReaderT . lift

-- Simplified memoize version when using a SNMapReaderT.
memoizeM :: MonadIO m => SNMapReaderT a m a -> SNMapReaderT a m a
memoizeM = memoize (SNMapReaderT get) (SNMapReaderT . put)

-- | Run a subcomputation in a scope, where nothing memoized inside will be remembered after
scopedM :: MonadIO m => SNMapReaderT a m x -> SNMapReaderT a m x
scopedM m = do
    save <- SNMapReaderT get
    x <- m
    SNMapReaderT $ put save
    return x
