{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators              #-}

module UserDbSpec where

import           Test.Hspec
import           Test.QuickCheck

import           Control.Monad.Except      (runExceptT)
import           Control.Monad.Reader      (runReaderT)

import           Servant
import           Database.Persist.Types      (Filter)
import           Database.Persist.Sql        (ConnectionPool, transactionUndo)
import           Database.Persist.Postgresql (Entity (..), fromSqlKey, insert,
                                              selectFirst, selectList, deleteWhere,
                                              (==.), runSqlPool)

import           Config (App (..), Config (..), Environment (..), makePool)
import           Models
import           Api.User


runAppToIO :: Config -> App a -> IO a
runAppToIO config app = do
  result <- runExceptT $ runReaderT (runApp app) config
  case result of
    Left err -> error $ show err
    Right a  -> return a

setupTeardown :: (Config -> IO a) -> IO ()
setupTeardown runTestsWith = do
  pool <- makePool Test
  migrateDb pool
  runTestsWith $ Config { getPool = pool, getEnv = Test }
  cleanDb pool
  where
    migrateDb :: ConnectionPool -> IO ()
    migrateDb pool = runSqlPool doMigrations pool

    cleanDb :: ConnectionPool -> IO ()
    cleanDb = deleteAllUsers

    deleteAllUsers :: ConnectionPool -> IO ()
    deleteAllUsers pool = do
      flip runSqlPool pool $ do
        deleteWhere ([] :: [Filter User])


-- for more detail, see `src/Config.hs`, but this assumes you have...
--   1. a Postgres `test` user
--   2. a `perservant-test` DB
spec :: Spec
spec = around setupTeardown $ do
  describe "User" $ do
    it "singleUser fetches User by name" $ \config -> do
      let user = User "username" "email"
      dbUser <- runAppToIO config $ do
        runDb $ insert user
        Entity _ user <- singleUser "username"
        return user
      dbUser `shouldBe` user
