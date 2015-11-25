{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Example.Connector where

import BasicPrelude
import Control.Monad.Trans.Resource
import Database.PostgreSQL.Connector
import Database.PostgreSQL.Simple ( Only )

basicQuery :: IO ()
basicQuery = do
  c <- newConn $ newConnInfo "postgres://localhost"
  runResourceT $ runConnectorT c $ do
    r <- query_ "select 2 + 2"
    liftIO $ print (r :: [Only Int])

