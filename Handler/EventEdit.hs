{-# LANGUAGE TupleSections, ScopedTypeVariables, QuasiQuotes, TemplateHaskell,
             MultiParamTypeClasses, OverloadedStrings, TypeFamilies #-}
module Handler.EventEdit (getEventEditR, postEventEditR) where

import Import
import Yesod.Auth
import Data.Maybe (fromJust, isNothing)
import Data.Time
import Database.Persist.Store (PersistValue (PersistInt64))
-- import Text.Blaze.Internal

-- Definimos una estructura que representan los datos del formulario
data EventEditForm = EventEditForm
  { eefSubject :: Text
  , eefDetail :: Text
  , eefNotified :: Bool
  , eefFireAt :: Day
  } deriving Show

-- Un constructor de formularios
eventEditForm subject detail notified fireAt = renderDivs $ EventEditForm
  <$> areq textField "Subject" subject
  <*> areq textField "Detail" detail
  <*> areq boolField "Notified" notified
  <*> areq dayField "FireAt" fireAt

getEventEditR :: Int -> Handler RepHtml
getEventEditR eventId = do
  userId <- requireAuthId
  let eventKey = Key . PersistInt64 $ fromIntegral eventId
      message = "entrada" :: Text

  eventPersist <- runDB $ selectFirst [EventUser ==. userId, EventId ==. eventKey] []
  let Entity eventDataId eventData = fromJust eventPersist
  now <- liftIO getCurrentTime
  let eventData' = EventEditForm (eventSubject  eventData)
                                 (eventDetail   eventData)
                                 (eventNotified eventData)
                                 (eventFireAt   eventData)
      emptyData = EventEditForm "" "" False $ utctDay now
      EventEditForm eventSubject'
                    eventDetail'
                    eventNotified'
                    eventFireAt' =
        if eventId == 0
          then emptyData
          else eventData'
  (form, formEnc) <- generateFormPost $ eventEditForm (Just eventSubject')
                                                      (Just eventDetail')
                                                      (Just eventNotified')
                                                      (Just eventFireAt')
  defaultLayout $ do
    setTitle "Edición de evento"
    $(widgetFile "eventedit")

postEventEditR :: Int -> Handler RepHtml
postEventEditR eventId' = do
  userId <- requireAuthId
  ((res, form), formEnc) <- runFormPost $ eventEditForm Nothing Nothing Nothing Nothing
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
                    if eventId' == 0
                      then do
                             eventKey' <- runDB $ insert $ Event userId
                                                                 (eefSubject  eData)
                                                                 (eefDetail   eData)
                                                                 (eefNotified eData)
                                                                 (eefFireAt   eData)
                             return eventKey'
                      else do
                             runDB $ update eventKey [ EventSubject  =. (eefSubject  eData)
                                                     , EventDetail   =. (eefDetail   eData)
                                                     , EventNotified =. (eefNotified eData)
                                                     , EventFireAt   =. (eefFireAt   eData)
                                                     ]
                             return eventKey
               _ -> return eventKey
  let Key (PersistInt64 eventId64) = eventKey''
      eventId = fromIntegral eventId64

  defaultLayout $ do
    setTitle "Edición de evento"
    $(widgetFile "eventedit")
