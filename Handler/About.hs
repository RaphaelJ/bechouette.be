module Handler.About (getAboutR) where

import Import

getAboutR :: Handler RepHtml
getAboutR = do
    extra <- getExtra
    setTitle "A propos de Be Chouette"
    defaultLayout $(widgetFile "about")
