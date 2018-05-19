{-# LANGUAGE OverloadedStrings #-}

module Init where

import qualified Control.Monad.Metrics       as M
import           Database.Persist.Postgresql (runSqlPool)
import           Network.Wai                 (Application)
import           Network.Wai.Handler.Warp    (Port)
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

-- | The 'initialize' function gathers the required environment information and
-- initializes the application, returning the port it should run on, the
-- 'Config', and the WAI 'Application' itself.
initialize :: IO (Port, Config, Application)
initialize = do
    env  <- lookupSetting "ENV" Development
    port <- lookupSetting "PORT" 8081
    logEnv <- defaultLogEnv
    pool <- makePool env logEnv
    store <- serverMetricStore <$> forkServer "localhost" 8000
    waiMetrics <- registerWaiMetrics store
    metr <- M.initializeWith store
    let cfg = Config { configPool = pool
                     , configEnv = env
                     , configMetrics = metr
                     , configLogEnv = logEnv }
        logger = setLogger env
    runSqlPool doMigrations pool
    generateJavaScript
    pure (port, cfg, logger $ metrics waiMetrics $ app cfg)

-- | When the 'Config' gains some state that may need to be released or
-- cleaned up, this function will take care of that.
shutdownApp :: Config -> IO ()
shutdownApp _ = pure ()

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

