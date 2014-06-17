{-# LANGUAGE OverloadedStrings #-}
module Handler.Home (
      getHomeR
    , listTopSubCats, listCats, subCatPic
    ) where

import Import
import Control.Monad

import Handler.Catalog (productPic)

getHomeR :: Handler Html
getHomeR = do
    (topSubCats, cats) <- runDB $ do
        topSubCats <- listTopSubCats
        cats       <- listCats
        return (zip [(0 :: Int)..] topSubCats, cats)

    defaultLayout $ do
        setTitle "Créations - Be Chouette"
        $(widgetFile "home")

listTopSubCats :: YesodDB App [(Entity SubCategory, Maybe (Entity Picture))]
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
        subCats <- selectList [SubCategoryCategory ==. catId]
                              [Asc SubCategoryName]
        subCatsPics <- forM subCats $ \subCatEntity@(Entity _ subCat) -> do
            mPic <- subCatPic subCat
            return (subCatEntity, mPic)
        return (cat, subCatsPics)

subCatPic :: SubCategory -> YesodDB App (Maybe (Entity Picture))
subCatPic subCat =
    case subCategoryMainProduct subCat of
        Just prodId -> productPic prodId
        Nothing     -> return Nothing
