{-# LANGUAGE OverloadedStrings #-}
module Handler.Home where

import Import
getHomeR :: Handler RepHtml
getHomeR = do
    defaultLayout $ do
        setTitle "Créations - Be Chouette"
        $(widgetFile "homepage")

