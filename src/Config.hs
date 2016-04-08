{-# LANGUAGE OverloadedStrings #-}

module Config where


import           Control.Monad.Logger                 (runNoLoggingT,
                                                       runStdoutLoggingT)
import           Control.Monad.Trans.Maybe            (MaybeT (..), runMaybeT)
import qualified Data.ByteString.Char8                as BS
import           Data.Monoid                          ((<>))
import           Network.Wai                          (Middleware)
import           Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)
import           System.Environment                   (lookupEnv)

import           Database.Persist.Postgresql          (ConnectionPool,
                                                       ConnectionString,
                                                       createPostgresqlPool)

data Config
    = Config
    { getPool :: ConnectionPool
    , getEnv  :: Environment
    }

data Environment
    = Development
    | Test
    | Production
    deriving (Eq, Show, Read)

setLogger :: Environment -> Middleware
setLogger Test = id
setLogger Development = logStdoutDev
setLogger Production = logStdout

makePool :: Environment -> IO ConnectionPool
makePool Test =
    runNoLoggingT (createPostgresqlPool (connStr Test) (envPool Test))
makePool Development =
    runStdoutLoggingT (createPostgresqlPool (connStr Development) (envPool Development))
makePool Production = do
    pool <- runMaybeT $ do
        let keys = fmap BS.pack
                   [ "host="
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
        let prodStr = mconcat . zipWith (<>) keys . fmap BS.pack $ envVars
        runStdoutLoggingT $ createPostgresqlPool prodStr (envPool Production)
    case pool of
         Nothing -> error "Database Configuration not present in environment."
         Just a -> return a


envPool :: Environment -> Int
envPool Test = 1
envPool Development = 1
envPool Production = 8

connStr :: Environment -> ConnectionString
connStr _ = "host=localhost dbname=perservant user=test password=test port=5432"
