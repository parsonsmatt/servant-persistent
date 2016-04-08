module Main where

import           Database.Persist.Postgresql (runSqlPool)
import           Network.Wai.Handler.Warp    (run)
import           System.Environment          (lookupEnv)

import           Api                         (app)
import           Config                      (Config (..), Environment (..),
                                              makePool, setLogger)
import           Models                      (doMigrations)


main :: IO ()
main = do
    env  <- lookupSetting "ENV" Development
    port <- lookupSetting "PORT" 8081
    pool <- makePool env
    let cfg = Config { getPool = pool, getEnv = env }
        logger = setLogger env
    runSqlPool doMigrations pool
    run port $ logger $ app cfg


lookupSetting :: Read a => String -> a -> IO a
lookupSetting env def = do
    p <- lookupEnv env
    return $ case p of Nothing -> def
                       Just a  -> read a
