module MasterPage.PublicLayout (publicLayout) where

import Import
import qualified Data.Text as T
import Network.Wai (remoteHost)
import Network.Socket (SockAddr)
import qualified Data.List as L

getIP :: SockAddr -> Text
getIP = L.head . T.splitOn ":" . T.pack . show

publicLayout pageContent = do
  ip <- fmap (getIP . remoteHost . reqWaiRequest) getRequest
  defaultLayout $ do
      $(widgetFile "publiclayout")
