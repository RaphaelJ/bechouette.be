module Handler.About (getAboutR) where

import Import

getAboutR :: Handler RepHtml
getAboutR = do
    extra <- getExtra
    defaultLayout $(widgetFile "about")
