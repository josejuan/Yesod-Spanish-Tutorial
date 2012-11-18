{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import
import MasterPage.PublicLayout (publicLayout)

getHomeR :: Handler RepHtml
getHomeR = do
    publicLayout $ do
        setTitle "Yesod Web Framework, tutorial en Español"
        $(widgetFile "homepage")
