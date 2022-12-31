{-# LANGUAGE OverloadedStrings #-}

module MyInit (
  (@/=), (@==), (==@)
  , asIO
  , assertNotEqual
  , assertNotEmpty
  , assertEmpty
  , BackendMonad
  , runConn

  , MonadIO
  , persistSettings
  , MkPersistSettings (..)
  , db
  , BackendKey(..)
  , GenerateKey(..)

  , RunDb
   -- re-exports
  , module Database.Persist
  , module Database.Persist.Sql.Raw.QQ
  , module Test.Hspec
  , module Test.HUnit
  , liftIO
  , mkPersist, mkMigrate, share, sqlSettings, persistLowerCase, persistUpperCase
  , Int32, Int64
  , Text
  , module Control.Monad.Trans.Reader
  , module Control.Monad
  , module Database.Persist.Sql
  , BS.ByteString
  , SomeException
  , MonadFail
  , TestFn(..)
  , truncateTimeOfDay
  , truncateToMicro
  , truncateUTCTime
  , arbText
  , liftA2
  ) where

import Init
    ( TestFn(..), truncateTimeOfDay, truncateUTCTime
    , truncateToMicro, arbText, GenerateKey(..)
    , (@/=), (@==), (==@)
    , assertNotEqual, assertNotEmpty, assertEmpty, asIO
    , RunDb, MonadFail
    )

-- re-exports
import Control.Applicative (liftA2)
import Control.Exception (SomeException)
import Control.Monad (void, replicateM, liftM, when, forM_)
import Control.Monad.Trans.Reader
import Database.Persist.TH (mkPersist, mkMigrate, share, sqlSettings, persistLowerCase, persistUpperCase, MkPersistSettings(..))
import Database.Persist.Sql.Raw.QQ
import Test.Hspec
import Test.QuickCheck.Instances ()

-- testing
import Test.HUnit ((@?=),(@=?), Assertion, assertFailure, assertBool)

import Control.Monad (unless, (>=>))
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Trans.Resource (ResourceT, runResourceT)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Int (Int32, Int64)
import Data.Text (Text)
import System.Environment (getEnv, lookupEnv)
import System.Log.FastLogger (fromLogStr)

import Database.Persist
import Database.Persist.MySQL
import Database.Persist.Sql
import Database.Persist.TH ()

persistSettings :: MkPersistSettings
persistSettings = sqlSettings { mpsGeneric = True }

type BackendMonad = SqlBackend

getEnvBS :: String -> IO BS.ByteString
getEnvBS k = BSC.pack <$> getEnv k

runConn :: MonadUnliftIO m => SqlPersistT (LoggingT m) t -> m ()
runConn f = do
  (hostname, database, username, password, logLevel) <- liftIO $ (,,,,)
    <$> getEnv "MYSQL_HOST"
    <*> getEnvBS "MYSQL_DATABASE"
    <*> getEnvBS "MYSQL_USER"
    <*> getEnvBS "MYSQL_PASSWORD"
    <*> lookupEnv "TEST_LOG"

  let printDebug = if (logLevel == Just "debug") then print . fromLogStr else void . return
  let ff = rawExecute "SET SESSION sql_mode = ''" [] >> f
  flip runLoggingT (\_ _ _ s -> printDebug s) $ do
    _ <- withMySQLPool (mkMySQLConnectInfo hostname username password database) 1 $ runSqlPool ff
    return ()

db :: SqlPersistT (LoggingT (ResourceT IO)) () -> Assertion
db actions = do
  runResourceT $ runConn $ actions >> transactionUndo
