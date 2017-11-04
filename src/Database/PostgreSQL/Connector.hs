{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE UndecidableInstances       #-}

-- |
-- Module:      Database.PostgreSQL.Connector
-- Copyright:   (c) 2015 Mark Fine
-- License:     BSD3
-- Maintainer:  Mark Fine <mark.fine@gmail.com>
--
-- PG Connector.

module Database.PostgreSQL.Connector
  ( ConnectorT (..)
  , Conn (..)
  , HasConn (..)
  , MonadConnector
  , ConnInfo (..)
  , runConnectorT
  , newConn
  , newConnInfo
  , withConnection
  , withTransaction
  , query
  , query_
  , execute
  , executeMany
  , execute_
  , returning
  ) where

import           Control.Lens
import           Control.Monad.Base
import           Control.Monad.Catch
import           Control.Monad.Reader
import           Control.Monad.Trans.Resource
import           Data.ByteString
import           Data.Int
import           Data.Pool
import           Data.Time
import qualified Database.PostgreSQL.Simple   as PG

newtype ConnectorT e m a = ConnectorT
  { unConnectorT :: ReaderT e m a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadCatch
             , MonadIO
             , MonadMask
             , MonadReader e
             , MonadThrow
             )

instance MonadBase b m => MonadBase b (ConnectorT r m) where
  liftBase = liftBaseDefault

instance MonadTrans (ConnectorT r) where
  lift = ConnectorT . lift

instance MonadResource m => MonadResource (ConnectorT r m) where
  liftResourceT = lift . liftResourceT

newtype Conn = Conn
  { _cConnectionPool :: Pool PG.Connection
  }

$(makeClassy ''Conn)

type MonadConnector e m =
  ( MonadIO       m
  , MonadMask     m
  , MonadResource m
  , MonadReader e m
  , HasConn     e
  )

data ConnInfo = ConnInfo
  { _ciDatabaseUrl :: ByteString
  , _ciStripes     :: Int
  , _ciConnections :: Int
  , _ciIdleTime    :: NominalDiffTime
  } deriving ( Eq, Show )

$(makeLenses ''ConnInfo)

runConnectorT :: HasConn e => e -> ConnectorT e m a -> m a
runConnectorT e (ConnectorT m) = runReaderT m e

newConn :: MonadIO m => ConnInfo -> m Conn
newConn ci =
  liftIO $ Conn <$> createPool
    (PG.connectPostgreSQL (ci ^. ciDatabaseUrl))
    PG.close
    (ci ^. ciStripes)
    (ci ^. ciIdleTime)
    (ci ^. ciConnections)

newConnInfo :: ByteString -> ConnInfo
newConnInfo databaseUrl =
  ConnInfo
    { _ciDatabaseUrl = databaseUrl
    , _ciStripes     = 1
    , _ciConnections = 2
    , _ciIdleTime    = 300
    }

connect :: MonadConnector e m => m (PG.Connection, LocalPool PG.Connection)
connect = do
  connectionPool <- view cConnectionPool
  liftIO $ takeResource connectionPool

restore :: MonadConnector e m => PG.Connection -> LocalPool PG.Connection -> m ()
restore connection connectionPool =
  liftIO $ putResource connectionPool connection

withConnection :: MonadConnector e m => (PG.Connection -> m a) -> m a
withConnection action =
  bracket connect (uncurry restore) $ uncurry $ \connection _connectionPool ->
    action connection

withTransaction :: MonadConnector e m => IO a -> m a
withTransaction action =
  withConnection $ \connection ->
    liftIO $ PG.withTransaction connection action

query :: (MonadConnector e m, PG.ToRow q, PG.FromRow r) => PG.Query -> q -> m [r]
query q params =
  withConnection $ \connection ->
    liftIO $ PG.query connection q params

query_ :: (MonadConnector e m, PG.FromRow r) => PG.Query -> m [r]
query_ q =
  withConnection $ \connection ->
    liftIO $ PG.query_ connection q

execute :: (MonadConnector e m, PG.ToRow q) => PG.Query -> q -> m Int64
execute q params =
  withConnection $ \connection ->
    liftIO $ PG.execute connection q params

executeMany :: (MonadConnector e m, PG.ToRow q) => PG.Query -> [q] -> m Int64
executeMany q params =
  withConnection $ \connection ->
    liftIO $ PG.executeMany connection q params

execute_ :: MonadConnector e m => PG.Query -> m Int64
execute_ q =
  withConnection $ \connection ->
    liftIO $ PG.execute_ connection q

returning :: (MonadConnector e m, PG.ToRow q, PG.FromRow r) => PG.Query -> [q] -> m [r]
returning q params =
  withConnection $ \connection ->
    liftIO $ PG.returning connection q params
