{-# LANGUAGE TupleSections, ScopedTypeVariables, QuasiQuotes, TemplateHaskell,
             MultiParamTypeClasses, OverloadedStrings, TypeFamilies #-}
module Handler.EventEdit (getEventEditR, postEventEditR) where

import Import
import Yesod.Auth
import Data.Maybe (fromMaybe, fromJust, isNothing)
import Control.Monad
import Data.Time
import System.Locale
import Database.Persist.Store (PersistValue (PersistInt64))

-- Definimos una estructura que representan los datos del formulario
data EventEditForm = EventEditForm
  { eefSubject :: Text
  , eefDetail :: Text
  , eefFireAt :: Day
  } deriving Show

-- Un constructor de formularios
eventEditForm subject detail fireAt = renderDivs $ EventEditForm
  <$> areq textField "Subject" subject
  <*> areq textField "Detail" detail
  <*> areq dayField "FireAt" fireAt

getEventEditR :: Int -> Handler RepHtml
getEventEditR eventId = do
  userId <- requireAuthId
  let eventKey = Key . PersistInt64 $ fromIntegral eventId
      message = "entrada" :: Text

  eventPersist <- runDB $ selectFirst [EventUser ==. userId, EventId ==. eventKey] []
  let Entity eventDataId eventData = fromJust eventPersist
  now <- liftIO getCurrentTime
  let eventData' = EventEditForm (eventSubject eventData)
                                 (eventDetail  eventData)
                                 (eventFireAt  eventData)
      emptyData = EventEditForm "" "" $ utctDay now
      EventEditForm eventSubject'
                    eventDetail'
                    eventFireAt' =
        if eventId == 0
          then emptyData
          else eventData'
  (form, formEnc) <- generateFormPost $ eventEditForm (Just eventSubject')
                                                      (Just eventDetail')
                                                      (Just eventFireAt')
  defaultLayout $ do
    setTitle "Edición de evento"
    $(widgetFile "eventedit")

postEventEditR :: Int -> Handler RepHtml
postEventEditR eventId' = do
  userId <- requireAuthId
  ((res, form), formEnc) <- runFormPost $ eventEditForm Nothing Nothing Nothing
  (fGuardar, fNuevo, fDeshacer, fEliminar, fVolver ) <- runInputPost $ (,,,,)
                                                          <$> iopt textField "Guardar"
                                                          <*> iopt textField "Nuevo"
                                                          <*> iopt textField "Deshacer"
                                                          <*> iopt textField "Eliminar"
                                                          <*> iopt textField "Volver"
  let eventKey = Key . PersistInt64 $ fromIntegral eventId'
      message :: Text = if isNothing fGuardar then "no guardar" else "si guardar"
  eventKey'' <- case res of
               FormSuccess eData -> do
                    eventPersist <- runDB $ selectFirst [EventUser ==. userId, EventId ==. eventKey] []
                    let Entity _ eventData = fromJust eventPersist
 
                    if eventId' == 0
                      then do
                             eventKey' <- runDB $ insert $ Event userId
                                                                 (eefSubject eData)
                                                                 (eefDetail  eData)
                                                                 (eefFireAt  eData)
                             return eventKey'
                      else do
                             runDB $ update eventKey [ EventSubject =. (eefSubject eData)
                                                     , EventDetail  =. (eefDetail  eData)
                                                     , EventFireAt  =. (eefFireAt  eData)
                                                     ]
                             return eventKey
               _ -> return eventKey
  let Key (PersistInt64 eventId64) = eventKey''
      eventId = fromIntegral eventId64

  defaultLayout $ do
    setTitle "Edición de evento"
    $(widgetFile "eventedit")
