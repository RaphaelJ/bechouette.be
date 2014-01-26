{-# LANGUAGE OverloadedStrings #-}
module Handler.Product (getProductR) where

import Import
import Prelude (head, tail)
import Text.Hamlet (shamlet)

getProductR :: ProductId -> Handler Html
getProductR prodId = do
    (prod, pics) <- runDB $ (,) <$> get404 prodId
                                <*> (selectList [PictureProduct ==. prodId]
                                                [Asc PictureId])

    defaultLayout $ do
        setTitle [shamlet|#{productName prod} - Be Chouette|]
        $(widgetFile "product")
        toWidgetHead [hamlet|
            <meta property="og:title" content="#{productName prod}" />
            <meta property="og:type" content="object" />
            <meta property="og:url" content="@{ProductR prodId}" />
            <meta property="og:description"content="#{productShortDesc prod}" />
            $if null pics
            $else
             $with Entity picId pic <- head pics
              $with picExt <- pictureExtension pic
                <meta property="og:image" content="@{routePicture picId PicLarge picExt}"/>
            |]
