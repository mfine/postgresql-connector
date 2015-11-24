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

module Database.PostgreSQL.Connector where

import           Control.Concurrent
import           Control.Lens
import           Control.Monad.Base
import           Control.Monad.Catch
import           Control.Monad.Reader
import           Control.Monad.Trans.Resource
import           Data.ByteString
import           Data.Int
import qualified Database.PostgreSQL.Simple as PG

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

data Conn = Conn
  { _connDatabaseUrl    :: ByteString
  , _connConnectionPool :: MVar [PG.Connection]
  }

$(makeClassy ''Conn)

type MonadConnector e m =
  ( MonadIO       m
  , MonadMask     m
  , MonadResource m
  , MonadReader e m
  , HasConn     e
  )

newConn :: MonadIO m => ByteString -> m Conn
newConn databaseUrl = do
  connectionPool <- liftIO $ newMVar []
  return Conn
    { _connDatabaseUrl    = databaseUrl
    , _connConnectionPool = connectionPool
    }

runConnectorT :: HasConn e => e -> ConnectorT e m a -> m a
runConnectorT e (ConnectorT m) = runReaderT m e

connect :: MonadConnector e m => m PG.Connection
connect = do
  connectionPool <- view connConnectionPool
  databaseUrl    <- view connDatabaseUrl
  liftIO $ modifyMVar connectionPool $ \connectionPool' ->
    case connectionPool' of
      (connection:connections) -> return (connections, connection)
      [] -> do
        connection <- PG.connectPostgreSQL databaseUrl
        return ([], connection)

restore :: MonadConnector e m => PG.Connection -> m ()
restore connection = do
  connectionPool <- view connConnectionPool
  liftIO $ modifyMVar_ connectionPool $ \connectionPool' ->
    return $ connection:connectionPool'

withConnection :: MonadConnector e m => (PG.Connection -> m a) -> m a
withConnection = bracket connect restore

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
