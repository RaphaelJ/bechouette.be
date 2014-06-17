module Handler.BirthList (
      getBirthListsR, getBirthListR, getBirthListProductR
    , listBirthLists, listBirthListProducts, birthListPic
    ) where

import Import
import Control.Monad
import Data.Function
import Data.List

import Handler.Catalog (productPic)

getBirthListsR :: Handler Html
getBirthListsR = do
    bls <- runDB $ listBirthLists

    defaultLayout $ do
        setTitle "Listes de naissance - Be Chouette"
        $(widgetFile "birthlists")

getBirthListR :: BirthListId -> Handler Html
getBirthListR blId = do
    (bl, prods, currProdId, currProd, currBlProd, currProdPics) <-
        runDB $ do
            bl    <- get404 blId
            prods <- listBirthListProducts blId

            when (null prods)
                notFound

            prods' <- forM prods $ \prod@(_, Entity prodId _) -> do
                pic <- productPic prodId
                return (prod, pic)

            let currProdId =
                    case birthListMainProduct bl of
                        Just prodId -> prodId
                        Nothing     -> entityKey $ snd $ head prods
            Just currBlProd <- getBy $ UniqueBirthListProduct blId currProdId
            currProd     <- getJust currProdId
            currProdPics <- selectList [PictureProduct ==. currProdId]
                                        [Asc PictureId]

            return (bl, prods', currProdId, currProd, currBlProd, currProdPics)

    displayBirthList (Entity blId bl) prods (Entity currProdId currProd)
                     currBlProd currProdPics

getBirthListProductR :: BirthListId -> BirthListProductId -> Handler Html
getBirthListProductR blId currBlProdId = do
    (bl, prods, currProdId, currProd, currBlProd, currProdPics) <- runDB $ do
        bl    <- get404 blId
        prods <- listBirthListProducts blId

        when (null prods)
            notFound

        prods' <- forM prods $ \prod@(_, Entity prodId _) -> do
            pic <- productPic prodId
            return (prod, pic)

        currBlProd <- get404 currBlProdId
        let currProdId = birthListProductProduct currBlProd
        currProd     <- getJust currProdId
        currProdPics <- selectList [PictureProduct ==. currProdId]
                                   [Asc PictureId]

        return (bl, prods', currProdId, currProd, currBlProd, currProdPics)

    displayBirthList (Entity blId bl) prods (Entity currProdId currProd)
                     (Entity currBlProdId currBlProd) currProdPics

displayBirthList :: Entity BirthList
              -> [((Entity BirthListProduct, Entity Product)
                   , Maybe (Entity Picture))]
              -> Entity Product -> Entity BirthListProduct -> [Entity Picture]
              -> HandlerT App IO Html
displayBirthList (Entity blId bl) prods (Entity currProdId currProd)
                 (Entity currBlProdId currBlProd) currProdPics =
    defaultLayout $ do
        setTitle [shamlet|#{productName currProd} - Liste de naissance de #{birthListName bl} - Be Chouette|]
        $(widgetFile "birthlist")
        $(widgetFile "product")
        toWidgetHead [hamlet|
            <meta property="og:title" content="#{productName currProd} - Liste de naissance de #{birthListName bl}" />
            <meta property="og:type" content="object" />
            <meta property="og:url" content="@{BirthListProductR blId currBlProdId}" />
            <meta property="og:description"content="#{productShortDesc currProd}" />
            $if null currProdPics
            $else
             $with Entity picId pic <- head currProdPics
              $with picExt <- pictureExtension pic
                <meta property="og:image" content="@{routePicture picId PicLarge picExt}"/>
            |]

listBirthLists :: YesodDB App [(Entity BirthList, Maybe (Entity Picture))]
listBirthLists = do
    bls <- selectList [] [Asc BirthListName]
    forM bls $ \blEntity@(Entity _ bl) -> do
        mPic <- birthListPic bl
        return (blEntity, mPic)

listBirthListProducts :: BirthListId
                      -> YesodDB App [(Entity BirthListProduct, Entity Product)]
listBirthListProducts blId = do
    blProds <- selectList [BirthListProductBirthList ==. blId] []

    prods <- forM blProds $ \blProdEntity@(Entity _ blProd) -> do
        let prodId = birthListProductProduct blProd
        prod <- getJust prodId
        return (blProdEntity, Entity prodId prod)

    return $ sortBy (compare `on` (productName . entityVal . snd)) prods

birthListPic :: BirthList -> YesodDB App (Maybe (Entity Picture))
birthListPic bl =
    case birthListMainProduct bl of
        Just prodId -> productPic prodId
        Nothing     -> return Nothing
