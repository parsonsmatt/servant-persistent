{-# LANGUAGE OverloadedStrings #-}

-- | Running your app inside GHCi.
--
-- To start up GHCi for usage with Yesod, first make sure you are in dev mode:
--
-- > cabal configure -fdev
--
-- Note that @yesod devel@ automatically sets the dev flag.
-- Now launch the repl:
--
-- > cabal repl --ghc-options="-O0 -fobject-code"
--
-- To start your app, run:
--
-- > :l DevelMain
-- > DevelMain.update
--
-- You can also call @DevelMain.shutdown@ to stop the app
--
-- You will need to add the foreign-store package to your .cabal file.
-- It is very light-weight.
--
-- If you don't use cabal repl, you will need
-- to run the following in GHCi or to add it to
-- your .ghci file.
--
-- :set -DDEVELOPMENT
--
-- There is more information about this approach,
-- on the wiki: https://github.com/yesodweb/yesod/wiki/ghci

module DevelMain where

import           Prelude

import           Control.Concurrent       (MVar, ThreadId, forkIO, killThread,
                                           newEmptyMVar, putMVar, takeMVar)
import           Control.Exception        (finally)
import           Control.Monad            ((>=>))
import           Data.IORef               (IORef, newIORef, readIORef,
                                           writeIORef)
import           Foreign.Store            (Store (..), lookupStore, readStore,
                                           storeAction, withStore)
import           GHC.Word                 (Word32)
import           Network.Wai.Handler.Warp (defaultSettings, runSettings,
                                           setPort)

import           Init                     (initialize, shutdownApp)

-- | Start or restart the server.
-- newStore is from foreign-store.
-- A Store holds onto some data across ghci reloads
update :: IO ()
update = do
    mtidStore <- lookupStore tidStoreNum
    case mtidStore of
      -- no server running
      Nothing -> do
          done <- storeAction doneStore newEmptyMVar
          tid <- start done
          _ <- storeAction (Store tidStoreNum) (newIORef tid)
          return ()
      -- server is already running
      Just tidStore -> restartAppInNewThread tidStore
  where
    doneStore :: Store (MVar ())
    doneStore = Store 0

    -- shut the server down with killThread and wait for the done signal
    restartAppInNewThread :: Store (IORef ThreadId) -> IO ()
    restartAppInNewThread tidStore = modifyStoredIORef tidStore $ \tid -> do
        killThread tid
        withStore doneStore takeMVar
        readStore doneStore >>= start


    -- | Start the server in a separate thread.
    start :: MVar () -- ^ Written to when the thread is killed.
          -> IO ThreadId
    start done = do
        (port, config, app) <- initialize
        forkIO (finally (runSettings (setPort port defaultSettings) app)
                        -- Note that this implies concurrency
                        -- between shutdownApp and the next app that is starting.
                        -- Normally this should be fine
                        (putMVar done () >> shutdownApp config))

-- | kill the server
shutdown :: IO ()
shutdown = do
    mtidStore <- lookupStore tidStoreNum
    case mtidStore of
      -- no server running
      Nothing -> putStrLn "no app running"
      Just tidStore -> do
          withStore tidStore $ readIORef >=> killThread
          putStrLn "App is shutdown"

tidStoreNum :: Word32
tidStoreNum = 1

modifyStoredIORef :: Store (IORef a) -> (a -> IO a) -> IO ()
modifyStoredIORef store f = withStore store $ \ref -> do
    v <- readIORef ref
    f v >>= writeIORef ref


