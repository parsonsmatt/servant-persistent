{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Network.Wai.Handler.Warp (run)
import           Config                   (configPort)
import           Init                     (initialize, acquireConfig)

-- | The 'main' function gathers the required environment information and
-- initializes the application.
main :: IO ()
main = do
    cfg <- acquireConfig
    app <- initialize cfg
    run (configPort cfg) app
