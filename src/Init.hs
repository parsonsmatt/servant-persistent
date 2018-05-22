{-# LANGUAGE OverloadedStrings #-}

module Init where

import qualified Control.Monad.Metrics       as M
import           Database.Persist.Postgresql (runSqlPool)
import           Network.Wai                 (Application)
import           Network.Wai.Metrics         (metrics, registerWaiMetrics)
import           System.Environment          (lookupEnv)
import           System.Remote.Monitoring    (forkServer, serverMetricStore)

import           Api                         (app)
import           Api.User                    (generateJavaScript)
import           Config                      (Config (..), Environment (..),
                                              makePool, setLogger)
import           Logger                      (defaultLogEnv)
import           Models                      (doMigrations)
import           Safe                        (readMay)
import           Control.Exception           (bracket)
import           Network.Wai.Handler.Warp    (run)


-- | An action that creates WAI 'Application' together with its resources
--   and tears it down on exit
runApp :: IO ()
runApp = bracket acquireConfig shutdownApp runApp
  where
    runApp config = run (configPort config) =<< initialize config

-- | The 'initialize' function gathers the required environment information and
-- initializes the WAI 'Application' and returns it
initialize :: Config -> IO Application
initialize cfg@(Config pool env _ _ _) = do
    waiMetrics <-
        registerWaiMetrics =<< serverMetricStore
        <$> forkServer "localhost" 8000
    let logger = setLogger env
    runSqlPool doMigrations pool
    generateJavaScript
    pure (logger $ metrics waiMetrics $ app cfg)

-- | Allocates resources for 'Config'
acquireConfig :: IO Config
acquireConfig = do
    port <- lookupSetting "PORT" 8081
    env  <- lookupSetting "ENV" Development
    logEnv <- defaultLogEnv
    pool <- makePool env logEnv
    store <- serverMetricStore <$> forkServer "localhost" 8000
    waiMetrics <- registerWaiMetrics store
    metr <- M.initializeWith store
    pure Config { configPool = pool
                , configEnv = env
                , configMetrics = metr
                , configLogEnv = logEnv
                , configPort = port }

-- | When the 'Config' gains some state that may need to be released or
-- cleaned up, this function will take care of that.
shutdownApp :: Config -> IO ()
shutdownApp _ = pure () -- todo: release resources allocated in acquireConfig

-- | Looks up a setting in the environment, with a provided default, and
-- 'read's that information into the inferred type.
lookupSetting :: Read a => String -> a -> IO a
lookupSetting env def = do
    maybeValue <- lookupEnv env
    case maybeValue of
        Nothing ->
            return def
        Just str ->
            maybe (handleFailedRead str) return (readMay str)
  where
    handleFailedRead str =
        error $ mconcat
            [ "Failed to read [["
            , str
            , "]] for environment variable "
            , env
            ]
