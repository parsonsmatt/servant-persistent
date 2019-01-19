{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Config where

import           Control.Concurrent                   (ThreadId)
import           Control.Exception                    (throwIO)
import           Control.Monad.Except                 (ExceptT, MonadError)
import           Control.Monad.IO.Class
import           Control.Monad.Logger                 (MonadLogger (..))
import           Control.Monad.Metrics                (Metrics, MonadMetrics,
                                                       getMetrics)
import           Control.Monad.Reader                 (MonadIO, MonadReader,
                                                       ReaderT, asks)
import           Control.Monad.Trans.Maybe            (MaybeT (..), runMaybeT)
import qualified Data.ByteString.Char8                as BS
import           Data.Monoid                          ((<>))
import           Database.Persist.Postgresql          (ConnectionPool,
                                                       createPostgresqlPool)
import           Network.Wai                          (Middleware)
import           Network.Wai.Handler.Warp             (Port)
import           Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)
import           Servant                              (ServantErr)
import           System.Environment                   (lookupEnv)

import           Logger

-- | This type represents the effects we want to have for our application.
-- We wrap the standard Servant monad with 'ReaderT Config', which gives us
-- access to the application configuration using the 'MonadReader'
-- interface's 'ask' function.
--
-- By encapsulating the effects in our newtype, we can add layers to the
-- monad stack without having to modify code that uses the current layout.
newtype AppT m a
    = AppT
    { runApp :: ReaderT Config (ExceptT ServantErr m) a
    } deriving
    ( Functor, Applicative, Monad, MonadReader Config, MonadError ServantErr
    , MonadIO
    )

type App = AppT IO

-- | The Config for our application is (for now) the 'Environment' we're
-- running in and a Persistent 'ConnectionPool'.
data Config
    = Config
    { configPool      :: ConnectionPool
    , configEnv       :: Environment
    , configMetrics   :: Metrics
    , configEkgServer :: ThreadId
    , configLogEnv    :: LogEnv
    , configPort      :: Port
    }

instance Monad m => MonadMetrics (AppT m) where
    getMetrics = asks Config.configMetrics

-- | Katip instance for @AppT m@
instance MonadIO m => Katip (AppT m) where
    getLogEnv = asks configLogEnv
    localLogEnv = error "not implemented"

-- | MonadLogger instance to use within @AppT m@
instance MonadIO m => MonadLogger (AppT m) where
    monadLoggerLog = adapt logMsg

-- | MonadLogger instance to use in @makePool@
instance MonadIO m => MonadLogger (KatipT m) where
    monadLoggerLog = adapt logMsg

-- | Right now, we're distinguishing between three environments. We could
-- also add a @Staging@ environment if we needed to.
data Environment
    = Development
    | Test
    | Production
    deriving (Eq, Show, Read)

-- | This returns a 'Middleware' based on the environment that we're in.
setLogger :: Environment -> Middleware
setLogger Test        = id
setLogger Development = logStdoutDev
setLogger Production  = logStdout

-- | Web request logger (currently unimplemented and unused). For inspiration
-- see ApacheLogger from wai-logger package.
katipLogger :: LogEnv -> Middleware
katipLogger env app req respond = runKatipT env $ do
    -- todo: log proper request data
    logMsg "web" InfoS "todo: received some request"
    liftIO $ app req respond

-- | This function creates a 'ConnectionPool'
makePool :: Environment -> LogEnv -> IO ConnectionPool
makePool environment logEnv = do
    maybeConnStr <- getConnStr
    case maybeConnStr of
        -- If we don't have a correct database configuration, we can't
        -- handle that in the program, so we throw an IO exception. This is
        -- one example where using an exception is preferable to 'Maybe' or
        -- 'Either'.
         Nothing -> throwIO (userError "Database Configuration not present in environment.")
         Just connStr -> runKatipT logEnv (createPostgresqlPool connStr (envPool environment))

-- | Attempt to produce a connection string from the environment
getConnStr :: IO (Maybe BS.ByteString)
getConnStr = runMaybeT $ do
    -- This function makes heavy use of the 'MaybeT' monad transformer, which
    -- might be confusing if you're not familiar with it. It allows us to
    -- combine the effects from 'IO' and the effect of 'Maybe' into a single
    -- "big effect", so that when we bind out of @MaybeT IO a@, we get an
    -- @a@. If we just had @IO (Maybe a)@, then binding out of the IO would
    -- give us a @Maybe a@, which would make the code quite a bit more
    -- verbose.
    let keys = [ "host="
                , "port="
                , "user="
                , "password="
                , "dbname="
                ]
        envs = [ "PGHOST"
                , "PGPORT"
                , "PGUSER"
                , "PGPASS"
                , "PGDATABASE"
                ]
    envVars <- traverse (MaybeT . lookupEnv) envs
    pure $ BS.intercalate " " . zipWith (<>) keys $ BS.pack <$> envVars

-- | The number of pools to use for a given environment.
envPool :: Environment -> Int
envPool Test        = 1
envPool Development = 1
envPool Production  = 8
