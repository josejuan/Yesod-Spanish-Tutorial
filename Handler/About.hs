{-# LANGUAGE TupleSections, OverloadedStrings, ScopedTypeVariables #-}
module Handler.About (getAboutR) where

import Import
import MasterPage.PublicLayout (publicLayout)
import Widget.W3CValidator

getAboutR :: Handler RepHtml
getAboutR = do
  getAboutMessage <- getExtra >>= return . aboutMessage
  publicLayout $ do
      setTitle "Acerca de Notify Me"
      $(widgetFile "about")
