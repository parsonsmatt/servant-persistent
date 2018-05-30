{-# LANGUAGE OverloadedStrings #-}

module Init where

import           Control.Concurrent          (killThread)
import qualified Control.Monad.Metrics       as M
import           Database.Persist.Postgresql (runSqlPool)
import           Lens.Micro                  ((^.))
import           Network.Wai                 (Application)
import           Network.Wai.Metrics         (metrics, registerWaiMetrics)
import           System.Environment          (lookupEnv)
import           System.Remote.Monitoring    (forkServer, serverMetricStore,
                                              serverThreadId)

import           Api                         (app)
import           Api.User                    (generateJavaScript)
import           Config                      (Config (..), Environment (..),
                                              makePool, setLogger)
import           Control.Exception           (bracket)
import qualified Data.Pool                   as Pool
import qualified Katip
import           Logger                      (defaultLogEnv)
import           Models                      (doMigrations)
import           Network.Wai.Handler.Warp    (run)
import           Safe                        (readMay)

-- | An action that creates a WAI 'Application' together with its resources,
--   runs it, and tears it down on exit
runApp :: IO ()
runApp = bracket acquireConfig shutdownApp runApp
  where
    runApp config = run (configPort config) =<< initialize config

-- | The 'initialize' function accepts the required environment information,
-- initializes the WAI 'Application' and returns it
initialize :: Config -> IO Application
initialize cfg = do
    waiMetrics <- registerWaiMetrics (configMetrics cfg ^. M.metricsStore)
    let logger = setLogger (configEnv cfg)
    runSqlPool doMigrations (configPool cfg)
    generateJavaScript
    pure . logger . metrics waiMetrics . app $ cfg

-- | Allocates resources for 'Config'
acquireConfig :: IO Config
acquireConfig = do
    port <- lookupSetting "PORT" 8081
    env  <- lookupSetting "ENV" Development
    logEnv <- defaultLogEnv
    pool <- makePool env logEnv
    ekgServer <- forkServer "localhost" 8000
    let store = serverMetricStore ekgServer
    waiMetrics <- registerWaiMetrics  store
    metr <- M.initializeWith store
    pure Config
        { configPool = pool
        , configEnv = env
        , configMetrics = metr
        , configLogEnv = logEnv
        , configPort = port
        , configEkgServer = serverThreadId ekgServer
        }

-- | Takes care of cleaning up 'Config' resources
shutdownApp :: Config -> IO ()
shutdownApp cfg = do
    Katip.closeScribes (configLogEnv cfg)
    Pool.destroyAllResources (configPool cfg)
    -- Monad.Metrics does not provide a function to destroy metrics store
    -- so, it'll hopefully get torn down when async exception gets thrown
    -- at metrics server process
    killThread (configEkgServer cfg)
    pure ()

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
