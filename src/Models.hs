{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Models where

-- import           Control.Monad.IO.Class  (liftIO)
-- import           Control.Monad.Logger    (runStderrLoggingT)
import           Control.Monad.Reader
-- import           Database.Persist
import           Database.Persist.Postgresql
import           Database.Persist.TH

import Config

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User
    name String
    email String
    deriving Show

Friendship
    user1Id UserId
    user2Id UserId
    UniqueFriendship user1Id user2Id
|]

doMigrations :: ReaderT SqlBackend IO ()
doMigrations = runMigration migrateAll
