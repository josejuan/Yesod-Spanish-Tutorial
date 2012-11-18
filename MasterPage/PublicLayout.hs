module MasterPage.PublicLayout (publicLayout) where

import Import

publicLayout pageContent = do
  defaultLayout $ do
      $(widgetFile "publiclayout")
