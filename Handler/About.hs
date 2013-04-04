module Handler.About (getAboutR) where

import Import

getAboutR :: Handler RepHtml
getAboutR = do
    extra <- getExtra
    defaultLayout $ do
        setTitle "A propos de Be Chouette"
        $(widgetFile "about")
