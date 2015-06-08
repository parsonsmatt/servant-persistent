{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api where

import Control.Monad.Reader         (ReaderT, runReaderT, lift)
import Control.Monad.Trans.Either   (EitherT, left)
import Network.Wai                  (Application)
import Database.Persist.Postgresql  (insert, selectList, Entity(..)
                                    ,fromSqlKey, (==.))
import Data.Int                     (Int64)
import Servant

import Config    (Config(..))
import Models -- (Person, userToPerson, EntityField(UserName))

type PersonAPI = 
         "users" :> Get '[JSON] [Person]
    :<|> "users" :> Capture "name" String :> Get '[JSON] Person
    :<|> "users" :> ReqBody '[JSON] Person :> Post '[JSON] Int64

type AppM = ReaderT Config (EitherT ServantErr IO)

userAPI :: Proxy PersonAPI
userAPI = Proxy

app :: Config -> Application
app cfg = serve userAPI (readerServer cfg)

readerServer :: Config -> Server PersonAPI
readerServer cfg = enter (readerToEither cfg) server

readerToEither :: Config -> AppM :~> EitherT ServantErr IO
readerToEither cfg = Nat $ \x -> runReaderT x cfg

server :: ServerT PersonAPI AppM
server = allPersons :<|> singlePerson :<|> createPerson

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

createPerson :: Person -> AppM Int64
createPerson p = do
    newPerson <- runDb $ insert $ User (name p) (email p)
    return $ fromSqlKey newPerson
