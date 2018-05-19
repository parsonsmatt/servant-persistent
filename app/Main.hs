{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Network.Wai.Handler.Warp (run)

import           Init                     (initialize)

-- | The 'main' function gathers the required environment information and
-- initializes the application.
main :: IO ()
main = do
    (port, _cfg, app) <- initialize
    run port app
