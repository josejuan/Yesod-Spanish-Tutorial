module Handler.EventList (getEventListR) where

import Import
import Yesod.Auth
import Database.Persist.Store (PersistValue (PersistInt64))
import Data.Text (pack)

getEventListR :: Handler RepHtml
getEventListR = do
  userId <- requireAuthId
  eventList <- runDB $ selectList [EventUser ==. userId] []
  let fromKey k = fromIntegral i where Key (PersistInt64 i) = k
      list = zip (cycle [pack "dataOdd", pack "dataEven"]) eventList
  defaultLayout $ do
    setTitle "Listado de eventos"
    $(widgetFile "eventlist")
