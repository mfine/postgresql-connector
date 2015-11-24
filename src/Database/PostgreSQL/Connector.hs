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

import Control.Lens
import Control.Monad.Base
import Control.Monad.Reader
import Control.Monad.Trans.Resource
import Data.Int
import qualified Database.PostgreSQL.Simple as PG

newtype ConnectorT e m a = ConnectorT
  { unConnectorT :: ReaderT e m a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadIO
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
  { _connConnection :: PG.Connection
  }

$(makeClassy ''Conn)

type MonadConnector e m =
  ( MonadIO       m
  , MonadResource m
  , MonadReader e m
  , HasConn     e
  )

runConnectorT :: HasConn e => e -> ConnectorT e m a -> m a
runConnectorT e (ConnectorT m) = runReaderT m e

withTransaction :: MonadConnector e m => IO a -> m a
withTransaction action = do
  connection <- view connConnection
  liftIO $ PG.withTransaction connection action

query :: (MonadConnector e m, PG.ToRow q, PG.FromRow r) => PG.Query -> q -> m [r]
query q params = do
  connection <- view connConnection
  liftIO $ PG.query connection q params

query_ :: (MonadConnector e m, PG.FromRow r) => PG.Query -> m [r]
query_ q = do
  connection <- view connConnection
  liftIO $ PG.query_ connection q

execute :: (MonadConnector e m, PG.ToRow q) => PG.Query -> q -> m Int64
execute q params = do
  connection <- view connConnection
  liftIO $ PG.execute connection q params

executeMany :: (MonadConnector e m, PG.ToRow q) => PG.Query -> [q] -> m Int64
executeMany q params = do
  connection <- view connConnection
  liftIO $ PG.executeMany connection q params

execute_ :: MonadConnector e m => PG.Query -> m Int64
execute_ q = do
  connection <- view connConnection
  liftIO $ PG.execute_ connection q

returning :: (MonadConnector e m, PG.ToRow q, PG.FromRow r) => PG.Query -> [q] -> m [r]
returning q params = do
  connection <- view connConnection
  liftIO $ PG.returning connection q params
