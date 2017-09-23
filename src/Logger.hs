{-# LANGUAGE OverloadedStrings #-}
module Logger (
    mkLogEnv,
    runKatipT,
    KatipT(..),
    LogEnv,
) where

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           System.IO              (stdout)

import           Katip                  as K
import           Katip.Core             as K
import           System.Log.FastLogger  (fromLogStr)

mkLogEnv :: IO LogEnv
mkLogEnv = do
    handleScribe <- mkHandleScribe ColorIfTerminal stdout InfoS V2
    liftM (registerScribe "stdout" handleScribe) $ --todo replace liftM with fmap
        initLogEnv "servant-persistent" "production"

instance MonadIO m => MonadLogger (KatipT m) where
    monadLoggerLog loc src lvl msg =
        logMsg "ns-std" InfoS $ logStr (fromLogStr $ toLogStr msg)
