{-# LANGUAGE OverloadedStrings #-}
module Handler.Product (getProductR) where

import Import
import Prelude (head, tail)
import Text.Hamlet (shamlet)

getProductR :: ProductId -> Handler RepHtml
getProductR prodId = do
    (prod, pics) <- runDB $ (,) <$> get404 prodId
                                <*> (selectList [PictureProduct ==. prodId]
                                                [Asc PictureId])

    defaultLayout $ do
        setTitle [shamlet|#{productName prod} - Be Chouette|]
        $(widgetFile "product")
