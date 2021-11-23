{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase, GADTs #-}
module Backend where

import Common.Route
import Obelisk.Backend
import Database.PostgreSQL.Simple
import Data.Text
import Obelisk.Route
import Snap.Core
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as A

getConn :: ConnectInfo
getConn  = ConnectInfo "ec2-3-229-166-245.compute-1.amazonaws.com"
                       5432
                       "zflepegyzmddsa"
                       "97c7bb390714c870152e905f151068c6538c8ebd9c3c2fc91534d640cf6ab220"
                       "d5vk84c2ci602v"

migrate :: Query
migrate = "CREATE TABLE IF NOT EXISTS cliente (id SERIAL PRIMARY KEY, nome TEXT NOT NULL)"

backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_run = \serve -> do
      dbcon <- connect getConn
      serve $ do
          \case
                BackendRoute_Cliente :/ () -> do
                    Just nome <- A.decode <$> readRequestBody 2000
                    liftIO $ do
                        execute_ dbcon migrate
                        execute dbcon "INSERT INTO cliente (nome) VALUES (?)" [nome :: Text]
                    modifyResponse $ setResponseStatus 200 "OK"
                _ -> return ()
  , _backend_routeEncoder = fullRouteEncoder
  }
