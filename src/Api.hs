{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators              #-}

module Api (app) where

import           Control.Monad.Except
import           Control.Monad.Reader        (ReaderT, runReaderT)
import           Control.Monad.Reader.Class
import           Data.Int                    (Int64)
import           Database.Persist.Postgresql (Entity (..), fromSqlKey, insert,
                                              selectFirst, selectList, (==.))
import           Network.Wai                 (Application)
import           Servant

import           Config                      (Config (..), App(..))
import           Models

type PersonAPI =
         "users" :> Get '[JSON] [Person]
    :<|> "users" :> Capture "name" String :> Get '[JSON] Person
    :<|> "users" :> ReqBody '[JSON] Person :> Post '[JSON] Int64


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

-- | This is the function we export to run our API. Given a 'Config', we
-- return a WAI 'Application' which any WAI compliant server can run.
app :: Config -> Application
app cfg = serve userAPI (readerServer cfg)

-- | This functions tells Servant how to run the 'App' monad with our
-- 'server' function.
readerServer :: Config -> Server PersonAPI
readerServer cfg = enter (convertApp cfg) server

-- | This function converts our 'App' monad into the @ExceptT ServantErr
-- IO@ monad that Servant's 'enter' function needs in order to run the
-- application. The ':~>' type is a natural transformation, or, in
-- non-category theory terms, a function that converts two type
-- constructors without looking at the values in the types.
convertApp :: Config -> App :~> ExceptT ServantErr IO
convertApp cfg = Nat (flip runReaderT cfg . runApp)
