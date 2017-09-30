{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators              #-}

module Api.User where

import           Control.Monad.Except        (MonadIO, liftIO)
import           Control.Monad.Reader        (ReaderT, runReaderT)
import qualified Control.Monad.Metrics as M
import           Data.Int                    (Int64)
import           Database.Persist.Postgresql (Entity (..), fromSqlKey, insert,
                                              selectFirst, selectList, (==.))
import           Network.Wai                 (Application)
import           Servant
import           Servant.JS                  (vanillaJS, writeJSForAPI)

import           Config                      (AppT (..), Config (..))
import           Models                      (User(User), runDb, userEmail, userName)
import qualified Models as Md
import           Data.Text                   (Text)
import           Lens.Micro                  ((^.))
import           Control.Monad.Metrics       (increment, metricsCounters)
import           Data.IORef                  (readIORef)
import qualified Data.HashMap.Lazy as LH
import qualified System.Metrics.Counter as C

type UserAPI =
         "users" :> Get '[JSON] [Entity User]
    :<|> "users" :> Capture "name" Text :> Get '[JSON] (Entity User)
    :<|> "users" :> ReqBody '[JSON] User :> Post '[JSON] Int64
    :<|> "metrics" :> Get '[JSON] (LH.HashMap Text Int64)

-- | The server that runs the UserAPI
userServer :: MonadIO m => ServerT UserAPI (AppT m)
userServer = allUsers :<|> singleUser :<|> createUser :<|> waiMetrics

-- | Returns all users in the database.
allUsers :: MonadIO m => AppT m [Entity User]
allUsers = do
    increment "allUsers"
    runDb (selectList [] [])

-- | Returns a user by name or throws a 404 error.
singleUser :: MonadIO m => Text -> AppT m (Entity User)
singleUser str = do
    increment "singleUser"
    maybeUser <- runDb (selectFirst [Md.UserName ==. str] [])
    case maybeUser of
         Nothing ->
            throwError err404
         Just person ->
            return person

-- | Creates a user in the database.
createUser :: MonadIO m => User -> AppT m Int64
createUser p = do
    increment "createUser"
    newUser <- runDb (insert (User (userName p) (userEmail p)))
    return $ fromSqlKey newUser

-- | Return wai metrics as JSON
waiMetrics :: MonadIO m => AppT m (LH.HashMap Text Int64)
waiMetrics = do
    increment "metrics"
    metr <- M.getMetrics
    liftIO $ mapM C.read =<< readIORef (metr ^. metricsCounters)

-- | Generates JavaScript to query the User API.
generateJavaScript :: IO ()
generateJavaScript =
    writeJSForAPI (Proxy :: Proxy UserAPI) vanillaJS "./assets/api.js"

