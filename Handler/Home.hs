{-# LANGUAGE OverloadedStrings #-}
module Handler.Home (getHomeR, listProducts) where

import Import
import Control.Monad

getHomeR :: Handler Html
getHomeR = do
    (topProds, cats) <- runDB $ do
        topProds <- listTopProds
        cats <- listProducts
        return (zip [(1 :: Int)..] topProds, cats)

    defaultLayout $ do
        setTitle "Créations - Be Chouette"
        $(widgetFile "home")

listTopProds :: YesodDB App [(Entity Product, Maybe (Entity Picture))]
listTopProds = do
    prods <- selectList [ProductTop ==. True] [Asc ProductName]
    forM prods $ \prod@(Entity prodId _) -> do
        mPic <- getPic prodId
        return (prod, mPic)

listProducts :: YesodDB App [
      (Entity Category, [(Entity Product, Maybe (Entity Picture))])
    ]
listProducts = do
    cats <- selectList [] [Asc CategoryOrder, Asc CategoryName]
    forM cats $ \cat@(Entity catId _) -> do
        prods <- selectList [ProductCategory ==. catId] [Asc ProductName]
        prodsPics <- forM prods $ \prod@(Entity prodId _) -> do
            mPic <- getPic prodId
            return (prod, mPic)
        return (cat, prodsPics)

getPic :: ProductId -> YesodDB App (Maybe (Entity Picture))
getPic prodId = selectFirst [PictureProduct ==. prodId] [Asc PictureId]
