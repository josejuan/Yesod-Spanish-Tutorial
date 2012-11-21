{-# LANGUAGE TupleSections, ScopedTypeVariables, QuasiQuotes, TemplateHaskell,
             MultiParamTypeClasses, OverloadedStrings, TypeFamilies #-}
module Handler.UserProfile (getUserProfileR, postUserProfileR) where

import Import
import Yesod.Auth
import Data.Maybe (fromJust, isNothing)

-- Definimos una estructura que representan los datos del formulario
data UserProfileForm = UserProfileForm
  { upfEmail :: Text
  , upfName :: Text
  , upfSubject :: Text
  } deriving Show

-- Un constructor de formularios
userProfileForm email name subject = renderDivs $ UserProfileForm
  <$> areq textField "Email" email
  <*> areq textField "Name" name
  <*> areq textField "Subject" subject

genUserProfileR :: GWidget App App () -> Enctype -> GHandler App App RepHtml
genUserProfileR form formEnc = defaultLayout $ do
                                 setTitle "Perfil de usuario"
                                 $(widgetFile "userprofile")

getUserProfileR :: Handler RepHtml
getUserProfileR = do
  userId <- requireAuthId
  userPersist <- runDB $ selectFirst [UserProfileUser ==. userId] []
  let Entity _ userData = fromJust userPersist
      userData' = UserProfileForm (userProfileEmail          userData)
                                  (userProfileName           userData)
                                  (userProfileDefaultSubject userData)
      emptyData = UserProfileForm "" "" ""
      UserProfileForm userProfileEmail'
                      userProfileName'
                      userProfileDefaultSubject' =
        if not (isNothing userPersist)
          then userData'
          else emptyData
  (form, formEnc) <- generateFormPost $ userProfileForm (Just userProfileEmail')
                                                        (Just userProfileName')
                                                        (Just userProfileDefaultSubject')
  genUserProfileR form formEnc

postUserProfileR :: Handler RepHtml
postUserProfileR = do
  userId <- requireAuthId
  ((res, form), formEnc) <- runFormPost $ userProfileForm Nothing Nothing Nothing
  case res of
    FormSuccess uData -> do
         userPersist <- runDB $ selectFirst [UserProfileUser ==. userId] []
         let Entity userDataId' _ = fromJust userPersist
         if isNothing userPersist
           then do
                  _ <- runDB $ insert $ UserProfile userId
                                                    (upfEmail   uData)
                                                    (upfName    uData)
                                                    (upfSubject uData)
                  return ()
           else runDB $ update userDataId' [ UserProfileEmail          =. (upfEmail   uData)
                                           , UserProfileName           =. (upfName    uData)
                                           , UserProfileDefaultSubject =. (upfSubject uData)
                                           ]
    _ -> return ()
  genUserProfileR form formEnc
