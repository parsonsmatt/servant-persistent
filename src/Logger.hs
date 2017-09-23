{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Logger (
    mkLogEnv,
    runKatipT,
    KatipT(..),
    LogEnv,
) where

import Control.Monad.Logger
import Control.Monad
import Control.Monad.IO.Class
import System.IO (stdout)
import Data.Text
import Data.String.Conv
import qualified Control.Monad.Logger as CML
import Data.Monoid

import Katip

mkLogEnv :: IO LogEnv
mkLogEnv = do
    handleScribe <- mkHandleScribe ColorIfTerminal stdout InfoS V2
    liftM (registerScribe "stdout" handleScribe) $
        initLogEnv "servant-persistent" "production"

instance MonadIO m => MonadLogger (KatipT m) where
    monadLoggerLog loc src lvl msg = return ()
        -- logMsg "ns-std" InfoS $ logStr msg

