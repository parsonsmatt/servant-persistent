{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Api where

import Data.Aeson
import Control.Monad.Reader
import Control.Monad.Trans.Either
import Network.Wai
import GHC.Generics
import Database.Persist.Postgresql
import Servant
import Servant.API

import Config
import Models

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
    users <- runDb $ selectList [] []
    let people = map (\(Entity _ y) -> userToPerson y) users
    return people

singlePerson :: String -> AppM Person
singlePerson str = do
    users <- runDb $ selectList [UserName ==. str] []
    let list = map (\(Entity _ y) -> userToPerson y) users
    case list of
         []     -> lift $ left err404
         (x:xs) -> return x
