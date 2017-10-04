{-# LANGUAGE OverloadedStrings #-}
module Logger
    ( adapt
    , defaultLogEnv
    , logMsg
    , runKatipT
    , KatipT(..)
    , Katip(..)
    , LogEnv
    , Severity(..)
    ) where

import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import qualified Control.Monad.Logger   as Logger
import           Katip
import qualified Katip
import qualified System.IO              as IO
import qualified System.Log.FastLogger  as FastLogger

defaultLogEnv :: IO LogEnv
defaultLogEnv = do
    handleScribe <- mkHandleScribe ColorIfTerminal IO.stdout DebugS V2
    registerScribe "stdout" handleScribe <$> initLogEnv "servant-persistent" "production"

fromLevel :: LogLevel -> Severity
fromLevel LevelDebug     = DebugS
fromLevel LevelInfo      = InfoS
fromLevel LevelWarn      = WarningS
fromLevel LevelError     = ErrorS
fromLevel (LevelOther _) = NoticeS

-- | Transforms Katip logMsg into monadLoggerLog to be used inside
-- MonadLogger monad
adapt :: (ToLogStr msg, Applicative m, Katip m)  =>
         (Namespace -> Severity -> Katip.LogStr -> m ()) ->
         Loc -> LogSource -> LogLevel -> msg -> m ()
adapt f _ src lvl msg =
    f ns (fromLevel lvl) $ logStr' msg
  where
    ns = Namespace [src]
    logStr' = Katip.logStr . FastLogger.fromLogStr . Logger.toLogStr
