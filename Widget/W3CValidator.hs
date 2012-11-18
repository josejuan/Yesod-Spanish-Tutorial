module Widget.W3CValidator where

import Import

w3cValidator :: GWidget sub master ()
w3cValidator = $(widgetFile "w3cValidator")
