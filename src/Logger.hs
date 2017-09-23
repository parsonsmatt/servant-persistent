{-# LANGUAGE OverloadedStrings #-}
module Logger (
    mkLogEnv,
    runKatipT,
    KatipT(..),
    Katip(..),
    LogEnv,
) where

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           System.IO              (stdout)

import           Katip                  as K

mkLogEnv :: IO LogEnv
mkLogEnv = do
    handleScribe <- mkHandleScribe ColorIfTerminal stdout InfoS V2
    liftM (registerScribe "stdout" handleScribe) $ --todo replace liftM with fmap
        initLogEnv "servant-persistent" "production"

