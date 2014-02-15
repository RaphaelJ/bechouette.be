{-# LANGUAGE OverloadedStrings #-}
module Handler.Home (getHomeR, subCatPic, productPic) where

import Import
import Control.Monad

getHomeR :: Handler Html
getHomeR = do
    (topSubCats, cats) <- runDB $ do
        topSubCats <- listTopSubCats
        cats       <- listCats
        return (zip [(0 :: Int)..] topSubCats, cats)

    defaultLayout $ do
        setTitle "Créations - Be Chouette"
        $(widgetFile "home")

listTopSubCats :: YesodDB App [(Entity Product, Maybe (Entity Picture))]
listTopSubCats = do
    subCats <- selectList [SubCategoryTop ==. True] []
    forM subCats $ \subCatEntity@(Entity _ subCat) -> do
        mPic <- subCatPic subCat
        return (subCatEntity, mPic)

listCats :: YesodDB App [
      (Entity Category, [(Entity SubCategory, Maybe (Entity Picture))])
    ]
listCats = do
    cats <- selectList [] [Asc CategoryOrder, Asc CategoryName]
    forM cats $ \cat@(Entity catId _) -> do
        subCats <- selectList [ProductCategory ==. catId] [Asc ProductName]
        subCatsPics <- forM subCats $ \subCatEntity@(Entity _ subCat) -> do
            mPic <- subCatPic subCat
            return (subCatEntity, mPic)
        return (cat, subCatsPics)

subCatPic :: Entity SubCategory -> YesodDB App (Maybe (Entity Picture))
subCatPic subCat
    | Just prodId <- subCategoryMainProduct subCat = productPic prodId
    | otherwise                                    = return Nothing

-- | Retourne la première image du produit.
productPic :: ProductId -> YesodDB App (Maybe (Entity Picture))
productPic prodId = selectFirst [PictureProduct ==. prodId] [Asc PictureId]
