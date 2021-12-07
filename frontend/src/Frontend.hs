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
import Data.Aeson

data Pagina = Pagina0 | Pagina1 | Pagina2 | Pagina3 | Pagina4 | Pagina5 | Pagina6

getPath :: R BackendRoute -> T.Text
getPath r = renderBackendRoute checFullREnc r

getProdReq :: Int -> XhrRequest ()
getProdReq pid = xhrRequest "GET" (getPath (BackendRoute_Buscar :/ pid)) def

getListReq :: XhrRequest ()
getListReq = xhrRequest "GET" (getPath (BackendRoute_Listar :/ ())) def

sendRequest :: ToJSON a => R BackendRoute -> a -> XhrRequest T.Text
sendRequest r dados = postJson (getPath r) dados

reqProd :: ( DomBuilder t m
           , Prerender js t m
           ) => m ()
reqProd = do
    nome <- inputElement def
    vl <- numberInput
    qt <- numberInput
    let prod = fmap (\((n,v),q) -> Produto 0 n v q) (zipDyn (zipDyn (_inputElement_value nome) vl) qt)
    (submitBtn,_) <- el' "button" (text "Inserir")
    let click = domEvent Click submitBtn
    let prodEvt = tag (current prod) click
    _ :: Dynamic t (Event t (Maybe T.Text)) <- prerender
        (pure never)
        (fmap decodeXhrResponse <$> performRequestAsync (sendRequest (BackendRoute_Produto :/ ()) <$> prodEvt))
    return ()

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
        (fmap decodeXhrResponse <$> performRequestAsync (sendRequest (BackendRoute_Cliente :/ ()) <$> nm ))
    return ()

data Acao = Perfil Int | Editar Int

tabProduto :: (PostBuild t m, DomBuilder t m) => Dynamic t Produto -> m (Event t Acao)
tabProduto pr = do
    el "tr" $ do
        el "td" (dynText $ fmap (T.pack . show . produtoId) pr)
        el "td" (dynText $ fmap (T.pack . show . produtoNome) pr)
        el "td" (dynText $ fmap (T.pack . show . produtoValor) pr)
        el "td" (dynText $ fmap (T.pack . show . produtoQt) pr)
        evt1 <- fmap (fmap (const Perfil)) (button "perfil")
        evt2 <- fmap (fmap (const Editar)) (button "editar")
        return (attachPromptlyDynWith (flip ($)) (fmap produtoId pr) (leftmost [evt1,evt2]))

editarPerfil :: ( DomBuilder t m
             , Prerender js t m
             , MonadHold t m
             , MonadFix m
             , PostBuild t m) => Int -> Workflow t m T.Text
editarPerfil pid = Workflow $ do
    btn <- button "mostrar"
    prod :: Dynamic t (Event t (Maybe Produto)) <- prerender
        (pure never)
        (fmap decodeXhrResponse <$> performRequestAsync (const (getProdReq pid) <$> btn))
    mdyn <- return (switchDyn prod)
    dynE <- return ((fromMaybe (Produto 0 "" 0 0)) <$> mdyn)

    nome <- inputElement $ def & inputElementConfig_setValue .~ (fmap produtoNome dynE)
    vl <- numberInputDyn (fmap produtoValor dynE)
    qt <- numberInputDyn (fmap produtoQt dynE)

    let prod = fmap(\((n,v),q) -> Produto 0 n v q) (zipDyn (zipDyn (_inputElement_value nome) vl) qt)
    submitBtn <- button "Editar"
    let prodEvt = tag (current prod) submitBtn
    _ :: Dynamic t (Event t (Maybe T.Text)) <- prerender
        (pure never)
        (fmap decodeXhrResponse <$> performRequestAsync (sendRequest (BackendRoute_Editar :/ pid) <$> prodEvt))
    return ("Editar: " <> (T.pack $ show pid), reqTabela <$ submitBtn)
    where
        novoInput x = inputElement $ def
            & inputElementConfig_elementConfig
            . elementConfig_initialAttributes .~ ("value" =: x)

pagPerfil :: ( DomBuilder t m
             , Prerender js t m
             , MonadHold t m
             , MonadFix m
             , PostBuild t m) => Int -> Workflow t m T.Text
pagPerfil pid = Workflow $ do
    btn <- button "mostrar"
    prod :: Dynamic t (Event t (Maybe Produto)) <- prerender
        (pure never)
        (fmap decodeXhrResponse <$> performRequestAsync (const ( getProdReq pid) <$> btn))
    mdyn <- holdDyn Nothing (switchDyn prod)
    dynP <- return ((fromMaybe (Produto 0 "" 0 0)) <$> mdyn)
    el "div" $ do
        el "div" (dynText $ fmap produtoNome dynP)
        el "div" (dynText $ fmap (T.pack . show . produtoValor) dynP)
        el "div" (dynText $ fmap (T.pack . show . produtoQt) dynP)
    ret <- button "voltar"
    return ("Perfil: " <> (T.pack $ show pid), reqTabela <$ ret)

reqTabela :: ( DomBuilder t m
             , Prerender js t m
             , MonadHold t m
             , MonadFix m
             , PostBuild t m) => Workflow t m T.Text
reqTabela = Workflow $ do
    btn <- button "Listar"
    prods :: Dynamic t (Event t (Maybe [Produto])) <- prerender
        (pure never)
        (fmap decodeXhrResponse <$> performRequestAsync (const getListReq <$> btn))
    evt <- return (fmap (fromMaybe []) $ switchDyn prods)
    dynP <- foldDyn (++) [] evt
    tb <- el "table" $ do
        el "thead" $ do
            el "tr" $ do
                el "th" (text "Id")
                el "th" (text "Nome")
                el "th" (text "Valor")
                el "th" (text "Qt")
                el "th" blank
                el "th" blank

        el "tbody" $ do
             simpleList dynP tabProduto
    tb' <- return $ switchDyn $ fmap leftmost tb
    return ("Listagem", escolherPag <$> tb')
    where
        escolherPag (Perfil pid) = pagPerfil pid
        escolherPag (Editar pid) = editarPerfil pid

reqLista :: ( DomBuilder t m
            , Prerender js t m
            , MonadHold  t m
            , MonadFix m
            , PostBuild t m) => m ()
reqLista = do
    r <- workflow reqTabela
    el "div" (dynText r)

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
        li4 <- clickLi Pagina4 "Exemplo4: Inserção ao bd"
        li5 <- clickLi Pagina5 "Exemplo5: Inserção Produto"
        li6 <- clickLi Pagina6 "Exemplo6: Listagem produto"
        return (leftmost [li1, li2, li3, li4, li5, li6])
    holdDyn Pagina0 evs

currPag :: (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m, Prerender js t m) => Pagina -> m ()
currPag p = do
    case p of
         Pagina0 -> blank
         Pagina1 -> bttnEvt
         Pagina2 -> sumEvt
         Pagina3 -> pagClick
         Pagina4 -> req
         Pagina5 -> reqProd
         Pagina6 -> reqLista

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

numberInput :: (DomBuilder t m, Num a, Read a) => m (Dynamic t a)
numberInput = do
      n <- inputElement $ def
        & inputElementConfig_initialValue .~ "0"
        & inputElementConfig_elementConfig
        . elementConfig_initialAttributes .~ ("type" =: "number")
      return $ fmap (fromMaybe 0 . readMaybe . T.unpack)
                (_inputElement_value n)

numberInputDyn :: (DomBuilder t m, Num a, Read a, Show a) => Event t a -> m (Dynamic t a)
numberInputDyn p = do
    val <- return (fmap (T.pack . show) p)
    n <- inputElement $ def
      & inputElementConfig_setValue .~ val
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
      el "title" $ text "P2"
      elAttr "link" ("href" =: static @"main.css" <> "type" =: "text/css" <> "rel" =: "stylesheet") blank
  , _frontend_body = do
      el "h1" $ text "Haskell P2"
      mainPag
  }
