{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api where

import Control.Monad.Reader         (ReaderT, runReaderT, lift)
import Control.Monad.Trans.Either   (EitherT, left)
import Network.Wai                  (Application)
import Database.Persist.Postgresql  (selectList, Entity(..), (==.))
import Servant

import Config                       (Config(..), runDb)
import Models                    -- (Person, userToPerson, EntityField(UserName))

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
