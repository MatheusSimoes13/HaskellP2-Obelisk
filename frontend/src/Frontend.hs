{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications, ScopedTypeVariables #-}

module Frontend where

import Control.Monad
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Language.Javascript.JSaddle (eval, liftJSM)
import Control.Monad.Fix
import Obelisk.Frontend
import Obelisk.Configs
import Obelisk.Route
import Obelisk.Generated.Static
import Data.Map (Map)
import Text.Read
import Reflex.Dom.Core
import Data.Maybe
import Common.Api
import Common.Route

data Pagina = Pagina0 | Pagina1 | Pagina2 | Pagina3 | Pagina4

getPath :: T.Text
getPath = renderBackendRoute checFullREnc $ BackendRoute_Cliente :/ ()

nomeRequest :: T.Text -> XhrRequest T.Text
nomeRequest s = postJson getPath (Cliente s)

req :: ( DomBuilder t m
       , Prerender js t m
       ) => m ()
req = do
    inputEl <- inputElement def
    (submitBtn,_) <- el' "button" (text "Inserir")
    let click = domEvent Click submitBtn
    let nm = tag (current $ _inputElement_value inputEl) click 
    _ :: Dynamic t (Event t (Maybe T.Text)) <- prerender
        (pure never)
        (fmap decodeXhrResponse <$> performRequestAsync (nomeRequest <$> nm))
    return ()

countClick :: DomBuilder t m => m (Event t Int)
countClick = do
    (ev, _) <- el' "button" (text "+")
    return $ ((const 1) <$> (domEvent Click ev))

pagClick :: (MonadHold t m, PostBuild t m, DomBuilder t m, MonadFix m) => m ()
pagClick = do
    ev <- countClick
    cter <- accumDyn (+) 0 ev
    el "div" (dynText (fmap (T.pack . show) cter))

clickLi :: (DomBuilder t m, PostBuild t m, MonadHold t m) => Pagina -> T.Text -> m (Event t Pagina)
clickLi p t = do
    (ev, _) <- el' "li" (elAttr "a" ("href" =: "#" ) (text t))
    return $ (const p) <$> (domEvent Click ev)

menuLi :: (DomBuilder t m, PostBuild t m, MonadHold t m) => m (Dynamic t Pagina)
menuLi = do
    evs <- el "ul" $ do
        li1 <- clickLi Pagina1 "Exemplo1: Reverso de Palavra"
        li2 <- clickLi Pagina2 "Exemplo2: Soma"
        li3 <- clickLi Pagina3 "Exemplo3: Cliques"
        li4 <- clickLi Pagina4 "Exemplo: Inserção ao bd"
        return (leftmost [li1,li2,li3])
    holdDyn Pagina0 evs

currPag :: (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m, Prerender js t m) => Pagina -> m ()
currPag p = do
    case p of
         Pagina0 -> blank
         Pagina1 -> bttnEvt
         Pagina2 -> sumEvt
         Pagina3 -> pagClick
         Pagina4 -> blank

mainPag :: (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m, Prerender js t m) => m ()
mainPag = do
  elAttr "div" ("class" =: "principal") $ do
    pagina <- el "div" menuLi
    dyn_ $ currPag <$> pagina

revText :: T.Text -> T.Text
revText t = T.pack (reverse (T.unpack t))

sumButton :: (DomBuilder t m, PostBuild t m, MonadHold t m)
            => m (Event t Double)
sumButton = do
    n1 <- numberInput
    text " "
    n2 <- numberInput
    text " "
    (e,_) <- el' "button" (text "Somar")
    return $ attachPromptlyDynWith const
                                    (zipDynWith (+) n1 n2)
                                    (domEvent Click e)

sumEvt :: (DomBuilder t m, PostBuild t m, MonadHold t m)
            => m()
sumEvt = do
    evt <- sumButton
    numero <- holdDyn 0 evt
    el "div" (dynText (fmap (T.pack . show) numero)) 

buttonClick :: (DomBuilder t m, PostBuild t m, MonadHold t m)
            => m (Event t T.Text)
buttonClick = do
    t <- inputElement def
    (e,_) <- el' "button" (text "OK")
    return $ attachPromptlyDynWith const
                                   (fmap revText (_inputElement_value t))
                                   (domEvent Click e)

bttnEvt :: (DomBuilder t m, PostBuild t m, MonadHold t m)
            => m ()
bttnEvt = do
    evt <- buttonClick
    texto <- holdDyn "" evt
    el "div" (dynText texto)

caixaSoma :: (PostBuild t m, DomBuilder t m) => m ()
caixaSoma = do
    n1 <- numberInput
    text " "
    n2 <- numberInput
    dynText (fmap (T.pack . show) (zipDynWith (+) n1 n2))

numberInput :: DomBuilder t m => m (Dynamic t Double)
numberInput = do
      n <- inputElement $ def
        & inputElementConfig_initialValue .~ "0"
        & inputElementConfig_elementConfig
        . elementConfig_initialAttributes .~ ("type" =: "number")
      return $ fmap (fromMaybe 0 . readMaybe . T.unpack)
                (_inputElement_value n)

caixas :: (DomBuilder t m, PostBuild t m) => m ()
caixas = el "div" $ do
  t <- inputElement def
  s <- inputElement def
  text " "
  dynText $ zipDynWith (<>) (_inputElement_value t) (_inputElement_value s)

listaAtr :: Map T.Text T.Text
listaAtr = "class" =: "class1" <> "id" =: "li2"

menu :: DomBuilder t m => m ()
menu = do
    el "div" $ do
      el "ul" $ do
        el "li" (text "Item 1 ")
        elAttr  "li" 
                listaAtr 
                (text "Item 2 ")
        el "li" (text "Item 3 ")
        el "li" (text "Item 4 ")

frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = do
      el "title" $ text "Obelisk Minimal Example"
      elAttr "link" ("href" =: static @"main.css" <> "type" =: "text/css" <> "rel" =: "stylesheet") blank
  , _frontend_body = do
      el "h1" $ text "Olá mundo!"
      mainPag
  }
