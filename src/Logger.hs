{-# LANGUAGE OverloadedStrings #-}
module Logger (
    mkLogEnv,
    runKatipT,
    KatipT(..),
    Katip(..),
    LogEnv,
) where

import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Logger       as ML

import           System.IO                  (stdout)
import           System.Log.FastLogger.Date (FormattedTime, newTimeCache,
                                             simpleTimeFormat)

import           Katip                      as K

mkLogEnv :: IO LogEnv
mkLogEnv = do
    handleScribe <- mkHandleScribe ColorIfTerminal stdout InfoS V2
    registerScribe "stdout" handleScribe <$> initLogEnv "servant-persistent" "production"
