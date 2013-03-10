{-# LANGUAGE OverloadedStrings #-}
module Handler.Home (getHomeR, listProducts) where

import Import
import Control.Monad

getHomeR :: Handler RepHtml
getHomeR = do
    (topProds, cats) <- runDB $ do
        topProds <- listTopProds
        cats <- listProducts
        return (zip [(1 :: Int)..] topProds, cats)

    defaultLayout $ do
        setTitle "Créations - Be Chouette"
        $(widgetFile "home")

listTopProds :: YesodDB sub App [(Entity Product, Maybe PictureId)]
listTopProds = do
    prods <- selectList [ProductTop ==. True] [Asc ProductName]
    forM prods $ \prod@(Entity prodId _) -> do
        mPicId <- getPic prodId
        return (prod, mPicId)

listProducts :: YesodDB sub App [
      (Entity Category, [(Entity Product, Maybe PictureId)])
    ]
listProducts = do
    cats <- selectList [] [Asc CategoryName]
    forM cats $ \cat@(Entity catId _) -> do
        prods <- selectList [ProductCategory ==. catId] [Asc ProductName]
        prodsPics <- forM prods $ \prod@(Entity prodId _) -> do
            mPicId <- getPic prodId
            return (prod, mPicId)
        return (cat, prodsPics)

getPic :: ProductId -> YesodDB sub App (Maybe PictureId)
getPic prodId =
    selectFirst [PictureProduct ==. prodId] [Asc PictureId] >>=
    return . (entityKey <$>)