{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Control.Monad.Metrics       as M
import           Database.Persist.Postgresql (runSqlPool)
import           Lens.Micro
import           Network.Wai.Handler.Warp    (run)
import           Network.Wai.Metrics
import           System.Environment          (lookupEnv)
import           System.Metrics              (newStore)
import           System.Remote.Monitoring    (forkServer, serverMetricStore)

import           Api                         (app)
import           Api.User                    (generateJavaScript)
import           Config                      (Config (..), Environment (..),
                                              makePool, setLogger)
import           Logger                      (mkLogEnv)
import           Models                      (doMigrations)
import           Safe                        (readMay)

-- | The 'main' function gathers the required environment information and
-- initializes the application.
main :: IO ()
main = do
    env  <- lookupSetting "ENV" Development
    port <- lookupSetting "PORT" 8081
    logEnv <- mkLogEnv
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
    run port $ logger $ metrics waiMetrics $ app cfg

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
