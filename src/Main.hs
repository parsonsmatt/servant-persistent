{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad.Trans.Either
import Data.Aeson
import GHC.Generics
import Network.Wai
import Network.Wai.Handler.Warp
import Control.Monad.Reader
import System.Environment (lookupEnv)
import Servant
import Database.Persist.Postgresql

import Config
import Models

main :: IO ()
main = do
    env  <- lookupSetting "ENV" Development
    port <- lookupSetting "PORT" 8081
    pool <- makePool env
    let cfg = defaultConfig { getPool = pool, getEnv = env }
        logger = setLogger env
    runSqlPool doMigrations pool
    run port $ logger $ app cfg

lookupSetting :: Read a => String -> a -> IO a
lookupSetting env def = do
    p <- lookupEnv env
    return $ case p of Nothing -> def
                       Just a  -> read a
type PersonAPI = 
         "users" :> Get '[JSON] [Person]
    :<|> "users" :> Capture "name" String :> Get '[JSON] Person

type AppM = ReaderT Config (EitherT ServantErr IO)

app :: Config -> Application
app cfg = serve userAPI (readerServer cfg)

readerServer cfg = enter (readerToEither cfg) server

readerToEither :: Config -> AppM :~> EitherT ServantErr IO
readerToEither cfg = Nat $ \x -> runReaderT x cfg

userAPI :: Proxy PersonAPI
userAPI = Proxy

server :: ServerT PersonAPI AppM
server = allPersons :<|> singlePerson

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

data Person = Person
    { name :: String
    , email :: String
    } deriving (Eq, Show, Generic)

instance ToJSON Person

userToPerson :: User -> Person
userToPerson User{..} = Person { name = userName, email = userEmail }
