{-# LANGUAGE OverloadedStrings #-}
module Logger
    ( adapt
    , logMsg
    , mkLogEnv
    , runKatipT
    , KatipT(..)
    , Katip(..)
    , LogEnv
    , Severity(..)
    ) where

import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Logger       as ML
import           Katip                      as K
import           System.IO                  (stdout)
import           System.Log.FastLogger      (fromLogStr)
import           System.Log.FastLogger.Date (FormattedTime, newTimeCache,
                                             simpleTimeFormat)

mkLogEnv :: IO LogEnv
mkLogEnv = do
    handleScribe <- mkHandleScribe ColorIfTerminal stdout DebugS V2
    registerScribe "stdout" handleScribe <$> initLogEnv "servant-persistent" "production"

fromLevel :: LogLevel -> Severity
fromLevel LevelDebug     = DebugS
fromLevel LevelInfo      = InfoS
fromLevel LevelWarn      = WarningS
fromLevel LevelError     = ErrorS
fromLevel (LevelOther _) = NoticeS

adapt :: (ToLogStr msg, Applicative m, Katip m)  =>
         (Namespace -> Severity -> K.LogStr -> m ()) ->
         Loc -> LogSource -> LogLevel -> msg -> m ()
adapt f _ src lvl msg =
    f ns (fromLevel lvl) $ logStr' msg
  where
    ns = Namespace [src]
    logStr' = logStr . fromLogStr . toLogStr
