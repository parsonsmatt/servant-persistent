{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Config where

import Control.Monad.Reader
import Control.Monad.Logger

import Database.Persist.Postgresql

data Config = Config 
    { getPool :: ConnectionPool
    , getEnv :: Environment
    }

defaultConfig :: Config
defaultConfig = Config
    { getPool = undefined
    , getEnv = Development
    }

runDb query = do
    pool <- asks getPool
    liftIO $ runSqlPool query pool

makePool :: Environment -> IO ConnectionPool
makePool Test = runNoLoggingT $ createPostgresqlPool (connStr Test) (envPool Test)
makePool e = runStdoutLoggingT $ createPostgresqlPool (connStr e) (envPool e)

envPool :: Environment -> Int
envPool Test = 1
envPool Development = 1
envPool Production = 8

connStr :: Environment -> ConnectionString
connStr _ = "host=localhost dbname=perservant user=test password=test port=5432"

data Environment = 
    Development
  | Test
  | Production
  deriving (Eq, Show, Read)

