{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Api (app) where

import           Control.Monad.Except
import           Data.Int             (Int64)
import           Servant              ((:<|>) ((:<|>)), (:~>) (NT),
                                       Proxy (Proxy), Raw, ServantErr, Server,
                                       enter, serve, serveDirectoryFileServer)
import           Servant.Server

import           Api.User             (UserAPI, userServer)
import           Config               (AppT (..), Config (..))
import           Control.Category     ((<<<), (>>>))

-- | This is the function we export to run our 'UserAPI'. Given
-- a 'Config', we return a WAI 'Application' which any WAI compliant server
-- can run.
userApp :: Config -> Application
userApp cfg = serve (Proxy :: Proxy UserAPI) (appToServer cfg)

-- | This functions tells Servant how to run the 'App' monad with our
-- 'server' function. @NT 'Handler'@ is a natural transformation that
-- effectively specialises app base monad to IO
appToServer :: Config -> Server UserAPI
appToServer cfg = enter (convertApp cfg >>> NT Handler) userServer

-- | This function converts our @'AppT' m@ monad into the @ExceptT ServantErr
-- m@ monad that Servant's 'enter' function needs in order to run the
-- application. The ':~>' type is a natural transformation, or, in
-- non-category theory terms, a function that converts two type
-- constructors without looking at the values in the types.
convertApp :: Config -> AppT m :~> ExceptT ServantErr m
convertApp cfg = runReaderTNat cfg <<< NT runApp

-- | Since we also want to provide a minimal front end, we need to give
-- Servant a way to serve a directory with HTML and JavaScript. This
-- function creates a WAI application that just serves the files out of the
-- given directory.
files :: Server Raw
files = serveDirectoryFileServer "assets"

-- | Just like a normal API type, we can use the ':<|>' combinator to unify
-- two different APIs and applications. This is a powerful tool for code
-- reuse and abstraction! We need to put the 'Raw' endpoint last, since it
-- always succeeds.
type AppAPI = UserAPI :<|> Raw

appApi :: Proxy AppAPI
appApi = Proxy

-- | Finally, this function takes a configuration and runs our 'UserAPI'
-- alongside the 'Raw' endpoint that serves all of our files.
app :: Config -> Application
app cfg =
    serve appApi (appToServer cfg :<|> files)
