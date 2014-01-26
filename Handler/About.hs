module Handler.About (getAboutR) where

import Import

getAboutR :: Handler Html
getAboutR = do
    extra <- getExtra
    defaultLayout $ do
        setTitle "A propos de Be Chouette"
        $(widgetFile "about")
