{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
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
import Common.Api
import Data.Aeson.Text

getConn :: ConnectInfo
getConn  = ConnectInfo "ec2-3-229-166-245.compute-1.amazonaws.com"
                       5432
                       "zflepegyzmddsa"
                       "97c7bb390714c870152e905f151068c6538c8ebd9c3c2fc91534d640cf6ab220"
                       "d5vk84c2ci602v"

migrate :: Query
migrate = "CREATE TABLE IF NOT EXISTS cliente (id SERIAL PRIMARY KEY, nome TEXT NOT NULL)"

migrateProd :: Query
migrateProd = "CREATE TABLE IF NOT EXISTS produtoo (id SERIAL PRIMARY KEY, nome TEXT NOT NULL, valor REAL NOT NULL, qt INTEGER NOT NULL)"

backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_run = \serve -> do
      dbcon <- connect getConn
      serve $ do
          \case
                BackendRoute_Listar :/ () -> method GET $ do
                    res :: [Produto] <- liftIO $ do
                        execute_ dbcon migrateProd
                        query_ dbcon "SELECT * FROM produtoo"
                    modifyResponse $ setResponseStatus 200 "OK"
                    writeLazyText (encodeToLazyText res)
                BackendRoute_Buscar :/ pid -> method GET $ do
                    res :: [Produto] <- liftIO $ do
                        execute_ dbcon migrateProd
                        query dbcon "SELECT * FROM produtoo WHERE id=?" (Only (pid :: Int))
                    if res /= [] then do
                        modifyResponse $ setResponseStatus 200 "OK"
                        writeLazyText (encodeToLazyText (Prelude.head res))
                    else
                        modifyResponse $ setResponseStatus 404 "NOT FOUND"
                BackendRoute_Produto :/ () -> method POST $ do
                    prod <- A.decode <$> readRequestBody 2000
                    case prod of
                         Just produto ->
                             liftIO $ do
                                 execute_ dbcon migrateProd
                                 execute dbcon "INSERT INTO produtoo (nome,valor,qt) VALUES (?,?,?)"
                                         (produtoNome produto, produtoValor produto, produtoQt produto)
                             modifyResponse $ setResponseStatus 200 "OK"
                         Nothing -> modifyResponse $ setResponseStatus 500 "ERRO"
                BackendRoute_Cliente :/ () -> method POST $ do
                    Just nome <- A.decode <$> readRequestBody 2000
                    liftIO $ do
                        execute_ dbcon migrate
                        execute dbcon "INSERT INTO cliente (nome) VALUES (?)" [nome :: Text]
                    modifyResponse $ setResponseStatus 200 "OK"
                _ -> return ()
  , _backend_routeEncoder = fullRouteEncoder
  }
