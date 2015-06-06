{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad.Trans.Either
import Control.Monad.Identity
import Data.Aeson
import GHC.Generics
import Network.Wai
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Handler.Warp
import Control.Monad.Reader
import System.Environment (lookupEnv)
import Servant
import Database.Persist.Postgresql

import Config
import Models

main :: IO ()
main = do
    env <- lookupSetting "ENV" Development
    port <- lookupSetting "PORT" 8081
    pool <- makePool env
    let cfg = Config { getPool = pool, getEnv = env }
    runSqlPool doMigrations pool
    run port $ logStdoutDev $ app cfg

lookupSetting :: Read a => String -> a -> IO a
lookupSetting env def = do
    p <- lookupEnv env
    return $ case p of Nothing -> def
                       Just a  -> read a

data Person = Person
    { name :: String
    , email :: String
    } deriving (Eq, Show, Generic)

instance ToJSON Person

userToPerson :: User -> Person
userToPerson User{..} = Person { name = userName, email = userEmail }

users :: [Person]
users = [Person "Matt" "parsonsmatt@gmail.com", Person "What" "hello@example.com"]

type PersonAPI = 
         "users" :> Get '[JSON] [Person]
    :<|> "users" :> Capture "name" String :> Get '[JSON] Person

server :: ServerT PersonAPI AppM
server = allPersons :<|> singlePerson

type AppM = ReaderT Config (EitherT ServantErr IO)

allPersons :: AppM [Person]
allPersons = do
    users <- runDb $ selectList ([] :: [Filter User]) []
    let people = map (\(Entity _ y) -> userToPerson y) users
    return people

singlePerson :: String -> AppM Person
singlePerson str = do
    users <- runDb $ selectList ([UserName ==. str] :: [Filter User]) []
    let list = map (\(Entity _ y) -> userToPerson y) users
    case list of
        [] -> lift $ left err404
        xs -> return $ head xs

userAPI :: Proxy PersonAPI
userAPI = Proxy

readerToEither :: Config -> AppM :~> EitherT ServantErr IO
readerToEither cfg = Nat $ \x -> runReaderT x cfg

readerServer cfg = enter (readerToEither cfg) server

app :: Config -> Application
app cfg = serve userAPI (readerServer cfg)
