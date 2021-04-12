-- {-# LANGUAGE GeneralizedNewtypeDeriving, ExistentialQuantification, RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.SNMap (
    -- SNMap,
    SNMapReaderT,
    runSNMapReaderT,
    -- newSNMap,
    -- memoize,
    memoizeM,
    scopedM
)where

import           Control.Applicative              (Applicative)
import           Control.Monad.Exception          (MonadAsyncException,
                                                   MonadException)
import           Control.Monad.IO.Class           (MonadIO, liftIO)
import           Control.Monad.Trans.Class        (MonadTrans (..))
import           Control.Monad.Trans.State.Strict (StateT (StateT), evalStateT,
                                                   get, put)
import qualified Data.HashTable.IO                as HT
import           System.Mem.StableName            (StableName, makeStableName)
import           System.Mem.Weak                  (addFinalizer)

{- A map (SN stands for stable name) to cache the results of computations
(m ends up being constrained to MonadIO m).
-}
newtype SNMap m a = SNMap (HT.BasicHashTable (StableName (m a)) a)

newSNMap :: IO (SNMap m a)
newSNMap = SNMap <$> HT.new

memoize :: MonadIO m
    => m (SNMap m a) -- An "IO call" to retrieve our cache.
    -> m a -- The "IO call" to execute and cache the result.
    -> m a -- The result being naturally also returned.
memoize getter m = do
    s <- liftIO $ makeStableName $! m -- Does forcing the evaluation make sense here (since we try to avoid it...)? Is it just the first level?
    (SNMap h) <- getter
    x <- liftIO $ HT.lookup h s
    case x of
        Just a -> return a
        Nothing -> do
            a <- m
            (SNMap h') <- getter --Need to redo because of scope (TODO Check it)
            liftIO $ HT.insert h' s a
            return a

-- An (IO) action producing a 'b' value while caching 'a' values along the way.
newtype SNMapReaderT a m b = SNMapReaderT (StateT (SNMap (SNMapReaderT a m) a) m b) deriving (Functor, Applicative, Monad, MonadIO, MonadException, MonadAsyncException)

runSNMapReaderT :: MonadIO m => SNMapReaderT a m b -> m b
runSNMapReaderT (SNMapReaderT m) = do h <- liftIO newSNMap
                                      evalStateT m h

instance MonadTrans (SNMapReaderT a) where
    lift = SNMapReaderT . lift

-- Simplified memoize version when using a SNMapReaderT.
memoizeM :: MonadIO m => SNMapReaderT a m a -> SNMapReaderT a m a
memoizeM = memoize (SNMapReaderT get)

-- | Run a subcomputation in a scope, where nothing memoized inside will be remembered after
scopedM :: MonadIO m => SNMapReaderT a m x -> SNMapReaderT a m x
scopedM m= do SNMap h <- SNMapReaderT get
              save <- liftIO $ HT.toList h
              x <- m
              h' <- liftIO $ HT.fromList save
              SNMapReaderT $ put (SNMap h')
              return x
