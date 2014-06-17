{-# LANGUAGE OverloadedStrings #-}
module Handler.Catalog (
      getSubCategoryR, getSubCategoryProductR
    , listSubCatProducts, productPic
    ) where

import Import
import Control.Monad
import Data.Function
import Data.List

getSubCategoryR :: SubCategoryId -> Handler Html
getSubCategoryR subCatId = do
    (subCat, prods, currProdId, currProd, currSubCatProdId, currProdPics) <-
        runDB $ do
            subCat <- get404 subCatId
            prods <- listSubCatProducts subCatId

            when (null prods)
                notFound

            prods' <- forM prods $ \prod@(_, Entity prodId _) -> do
                pic <- productPic prodId
                return (prod, pic)

            let currProdId =
                    case subCategoryMainProduct subCat of
                        Just prodId -> prodId
                        Nothing     -> entityKey $ snd $ head prods
            Just (Entity currSubCatProdId _) <-
                getBy $ UniqueSubCategoryProduct subCatId currProdId
            currProd     <- getJust currProdId
            currProdPics <- selectList [PictureProduct ==. currProdId]
                                       [Asc PictureId]

            return (subCat, prods', currProdId, currProd, currSubCatProdId,
                    currProdPics)

    displaySubCat (Entity subCatId subCat) prods (Entity currProdId currProd)
                  currSubCatProdId currProdPics

getSubCategoryProductR :: SubCategoryId -> SubCategoryProductId -> Handler Html
getSubCategoryProductR subCatId currSubCatProdId = do
    (subCat, prods, currProdId, currProd, currProdPics) <- runDB $ do
        subCat <- get404 subCatId
        prods <- listSubCatProducts subCatId

        when (null prods)
            notFound

        prods' <- forM prods $ \prod@(_, Entity prodId _) -> do
            pic <- productPic prodId
            return (prod, pic)

        currSubCatProd <- get404 currSubCatProdId
        let currProdId = subCategoryProductProduct currSubCatProd
        currProd     <- getJust currProdId
        currProdPics <- selectList [PictureProduct ==. currProdId]
                                   [Asc PictureId]

        return (subCat, prods', currProdId, currProd, currProdPics)

    displaySubCat (Entity subCatId subCat) prods (Entity currProdId currProd)
                  currSubCatProdId currProdPics

displaySubCat :: Entity SubCategory 
              -> [((Entity SubCategoryProduct, Entity Product)
                   , Maybe (Entity Picture))]
              -> Entity Product -> SubCategoryProductId -> [Entity Picture]
              -> HandlerT App IO Html
displaySubCat (Entity subCatId subCat) prods (Entity currProdId currProd)
              currSubCatProdId currProdPics =
    defaultLayout $ do
        setTitle [shamlet|#{productName currProd} - #{subCategoryName subCat} - Be Chouette|]
        $(widgetFile "subcategory")
        $(widgetFile "product")
        toWidgetHead [hamlet|
            <meta property="og:title" content="#{productName currProd} - #{subCategoryName subCat}" />
            <meta property="og:type" content="object" />
            <meta property="og:url" content="@{SubCategoryProductR subCatId currSubCatProdId}" />
            <meta property="og:description"content="#{productShortDesc currProd}" />
            $if null currProdPics
            $else
             $with Entity picId pic <- head currProdPics
              $with picExt <- pictureExtension pic
                <meta property="og:image" content="@{routePicture picId PicLarge picExt}"/>
            |]

listSubCatProducts :: SubCategoryId
                   -> YesodDB App [(Entity SubCategoryProduct, Entity Product)]
listSubCatProducts subCatId = do
    subCatProds <- selectList [SubCategoryProductSubCategory ==. subCatId] []

    prods <- forM subCatProds $ \subCatProdEntity@(Entity _ subCatProd) -> do
        let prodId = subCategoryProductProduct subCatProd
        prod <- getJust prodId
        return (subCatProdEntity, Entity prodId prod)

    return $ sortBy (compare `on` (productName . entityVal . snd)) prods

-- | Retourne la première image du produit.
productPic :: ProductId -> YesodDB App (Maybe (Entity Picture))
productPic prodId = selectFirst [PictureProduct ==. prodId] [Asc PictureId]
