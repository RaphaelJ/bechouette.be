{-# LANGUAGE OverloadedStrings #-}
module Handler.Home (getHomeR, listProducts) where

import Import
import Control.Monad

getHomeR :: Handler RepHtml
getHomeR = do
    cats <- runDB listProducts

    defaultLayout $ do
        setTitle "Créations - Be Chouette"
        $(widgetFile "homepage")

listProducts :: YesodDB sub App [(Entity Category, [Entity Product])]
listProducts = do
    cats <- selectList [] [Asc CategoryName]
    forM cats $ \cat@(Entity catId _) -> do
        prods <- selectList [ProductCategory ==. catId] [Asc ProductName]
        return (cat, prods)