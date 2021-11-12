{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Frontend where

import Control.Monad
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Language.Javascript.JSaddle (eval, liftJSM)

import Obelisk.Frontend
import Obelisk.Configs
import Obelisk.Route
import Obelisk.Generated.Static

import Reflex.Dom.Core

import Common.Api
import Common.Route

caixas :: (DomBuilder t m, PostBuild t m) => m ()
caixas = el "div" $ do
  t <- inputElement def
  s <- inputElement def
  text " "
  dynText $ zipDynWith (<>) (_inputElement_value t) (_inputElement_value s)

menu :: DomBuilder t m => m ()
menu = do
    el "div" $ do
      el "ul" $ do
        el "li" (text "Item 1 ")
        elAttr "li" ("class" =: "class1")   ( text "Item 2 ")
        el "li" (text "Item 3 ")
        el "li" (text "Item 4 ")

frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = do
      el "title" $ text "Obelisk Minimal Example"
      elAttr "link" ("href" =: static @"main.css" <> "type" =: "text/css" <> "rel" =: "stylesheet") blank
  , _frontend_body = do
      el "h1" $ text "Olá mundo!"
      caixas
      el "p" $ text $ T.pack commonStuff

      elAttr "img" ("src" =: static @"obelisk.jpg") blank
      el "div" $ do
        exampleConfig <- getConfig "common/example"
        case exampleConfig of
          Nothing -> text "No config file found in config/common/example"
          Just s -> text $ T.decodeUtf8 s
      return ()
  }
