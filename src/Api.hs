{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators #-}

module Api where

import Control.Monad.Except
import Control.Monad.Reader         (ReaderT, runReaderT)
import Control.Monad.Reader.Class
import Network.Wai                  (Application)
import Database.Persist.Postgresql  (insert, selectList, Entity(..)
                                    ,fromSqlKey, (==.), selectFirst)
import Data.Int                     (Int64)
import Servant
import Servant.Server

import Config    (Config(..))
import Models -- (Person, userToPerson, EntityField(UserName))

type PersonAPI = 
         "users" :> Get '[JSON] [Person]
    :<|> "users" :> Capture "name" String :> Get '[JSON] Person
    :<|> "users" :> ReqBody '[JSON] Person :> Post '[JSON] Int64

newtype App a 
    = App 
    { runApp :: ReaderT Config (ExceptT ServantErr IO) a 
    } deriving ( Functor, Applicative, Monad, MonadReader Config, 
                 MonadError ServantErr, MonadIO)

userAPI :: Proxy PersonAPI
userAPI = Proxy

app :: Config -> Application
app cfg = serve userAPI (readerServer cfg)

readerServer :: Config -> Server PersonAPI
readerServer cfg = enter (convertApp cfg) server

convertApp :: Config -> App :~> ExceptT ServantErr IO
convertApp cfg = Nat (flip runReaderT cfg . runApp)

server :: ServerT PersonAPI App
server = allPersons :<|> singlePerson :<|> createPerson

allPersons :: App [Person]
allPersons = do
    users <- runDb (selectList [] [])
    let people = map (userToPerson . entityVal) users
    return people

singlePerson :: String -> App Person
singlePerson str = do
    maybeUser <- runDb (selectFirst [UserName ==. str] [])
    let maybePerson = fmap (userToPerson . entityVal) maybeUser
    case maybePerson of
         Nothing     -> throwError err404
         Just person -> return person

createPerson :: Person -> App Int64
createPerson p = do
    newPerson <- runDb (insert (User (name p) (email p)))
    return $ fromSqlKey newPerson
