{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Handler.Admin (
      getAdminLoginR, postAdminLoginR
    , getAdminProductsR, postAdminProductsR, getAdminProductR
    , postAdminProductR, postAdminProductRemoveR
    , getAdminPicturesR, postAdminPicturesR, getAdminRemovePictureR
      
    , getAdminEditCatR, postAdminEditCatR, getAdminRemoveCatR
    , getAdminNewProdR, postAdminNewProdR, getAdminEditProdR, postAdminEditProdR
    ) where

import Import
import Prelude (tail)
import Control.Applicative ((<|>))
import Control.Monad
import Data.Ratio
import Database.Persist.Sql (askSqlConn, connPrepare, connRollback)
import qualified Data.Text as T
import System.Directory
import System.FilePath (takeExtension)
import qualified Vision.Image as I
import Vision.Primitive (Z (..), (:.) (..), Rect (..))
import Text.Printf

import Handler.Home (listProducts)
import Handler.BirthLists (listBirthLists)

sessionKey :: Text
sessionKey = "CONNECTED"

getAdminLoginR :: Handler Html
getAdminLoginR = do
    redirectIfConnected

    (widget, enctype) <- generateFormPost loginForm

    let err = Nothing :: Maybe Text
    defaultLayout $ do
        setTitle "Connexion à l'administration - Be Chouette"
        $(widgetFile "admin-login")

postAdminLoginR :: Handler Html
postAdminLoginR = do
    redirectIfConnected

    ((result, widget), enctype) <- runFormPost loginForm

    validPass <- extraPassword <$> getExtra
    case result of
        FormSuccess pass | pass == validPass -> do
            setSession sessionKey ""
            redirect AdminR
        _ -> do
            let err = Just "Mot de passe invalide." :: Maybe Text
            defaultLayout $ do
                setTitle "Connexion à l'administration - Be Chouette"
                $(widgetFile "admin-login")

loginForm :: Form Text
loginForm = renderDivs $ areq passwordField "Mot de passe" Nothing

redirectIfConnected :: Handler ()
redirectIfConnected = do
    v <- lookupSession sessionKey
    case v of
        Just _  -> redirect AdminProductsR
        Nothing -> return ()

-- Produits --------------------------------------------------------------------

-- | Liste des produits.
getAdminProductsR :: Handler Html
getAdminProductsR = do
    redirectIfNotConnected

    (widget, enctype) <- generateFormPost (prodForm Nothing)

    prods <- runDB $ selectList [] [Asc ProductName]

    let err = Nothing
    defaultLayout $ do
        setTitle "Gestion des produits - Be Chouette"
        $(widgetFile "admin-products")

-- | Nouveau produit.
postAdminProductsR :: Handler Html
postAdminProductsR = do
    redirectIfNotConnected

    ((result, widget), enctype) <- runFormPost (prodForm Nothing)

    prods <- runDB $ selectList [] [Asc ProductName]

    err <- case result of
        FormSuccess prod -> do
            m <- runDB $ insertUnique prod
            case m of
                Just _  -> return $! Just ("Nom de produit existant." :: Text)
                Nothing -> redirect AdminProductsR
        _               -> return Nothing

    defaultLayout $ do
        setTitle "Ajouter un nouveau produit - Be Chouette"
        $(widgetFile "admin-products")

getAdminEditProdR :: ProductId -> Handler Html
getAdminEditProdR prodId = do
    redirectIfNotConnected

    prod <- runDB $ get404 prodId

    (widget, enctype) <- generateFormPost (prodForm (Just prod))

    defaultLayout $ do
        setTitle "Modifier un produit - Be Chouette"
        $(widgetFile "admin-product")

postAdminEditProdR :: ProductId -> Handler Html
postAdminEditProdR prodId = do
    redirectIfNotConnected

    prod <- runDB $ get404 prodId

    ((result, widget), enctype) <- runFormPost (prodForm (Just prod))

    case result of
        FormSuccess newProd -> do
            _ <- runDB $ replace prodId newProd
            redirect AdminProductsR
        _ -> do
            defaultLayout $ do
                setTitle "Modifier un produit - Be Chouette"
                $(widgetFile "admin-product")

getAdminProductRemoveR :: ProductId -> Handler ()
getAdminProductRemoveR prodId = do
    redirectIfNotConnected

    runDB $ removeProduct prodId
    redirect AdminProductsR

-- | Supprime les produits et ses dépendances.
removeProduct :: ProductId -> YesodDB App ()
removeProduct prodId = do
    delete prodId

    pics <- selectList [PictureProduct ==. prodId] []
    forM_ pics $ \(Entity picId pic) -> do
        removePicture picId (pictureExtension pic)

    subcats <- selectList [SubCategoryProductProduct ==. prodId] []
    forM_ subcats $ delete . entityKey

    birthlists <- selectList [BirthListProductProduct ==. prodId] []
    forM_ birthlists $ delete . entityKey

prodForm :: Maybe Product -> Form Product
prodForm mCatId prod html =
    flip renderDivs html $ Product
        <$> areq textField "Nom du produit" (productName <$> prod)
        <*> aopt textField "Référence du produit (facultatif)"
                (productRef <$> prod)
        <*> areq textField "Description rapide (pour catalogue et Facebook)"
                (productShortDesc <$> prod)
        <*> areq textareaField "Description complète (pour la fiche produit)"
                (productDesc <$> prod)
        <*> areq textareaField "Détails (tailles, couleurs, lavage, ...)"
                (productDetails <$> prod)
        <*> aopt doubleField
                "Prix (facultatif, '.' pour séparer les décimales)"
                (productPrice <$> prod)

-- Photos ----------------------------------------------------------------------

getAdminPicturesR :: ProductId -> Handler Html
getAdminPicturesR prodId = do
    redirectIfNotConnected

    (widget, enctype) <- generateFormPost pictureForm

    (prod, pics) <- runDB $ do
        prod <- get404 prodId
        pics <- selectList [PictureProduct ==. prodId] [Asc PictureId]
        return (prod, pics)

    let err = Nothing :: Maybe Text
    defaultLayout $ do
        setTitle "Gestion des photos - Be Chouette"
        $(widgetFile "admin-pictures")

postAdminPicturesR :: ProductId -> Handler Html
postAdminPicturesR prodId = do
    redirectIfNotConnected

    ((result, widget), enctype) <- runFormPost pictureForm

    err <- case result of
        FormSuccess info -> do
            mErr <- processImage info
            case mErr of
                Just err -> return $ Just err
                Nothing  -> redirect $ AdminPicturesR prodId
        _ -> return Nothing

    (prod, pics) <- runDB $ do
        prod <- get404 prodId
        pics <- selectList [PictureProduct ==. prodId] [Asc PictureId]
        return (prod, pics)

    defaultLayout $ do
        setTitle "Gestion des photos - Be Chouette"
        $(widgetFile "admin-pictures")
  where
    processImage info = do
        let picExt = T.pack $ tail $ takeExtension $ T.unpack $ fileName info
        runDB $ do
            picId <- insert (Picture prodId picExt)

            let path = picPath picId PicOriginal picExt
            liftIO $ fileMove info path

            mIo <- liftIO $ I.load path Nothing
            case mIo of
                Right io -> do
                    let img = I.convert io :: I.RGBImage
                    resizeImage picId img PicCatalogue picExt
                    resizeImage picId img PicSmall     picExt
                    resizeImage picId img PicLarge     picExt
                    resizeImage picId img PicWide      picExt
                    return Nothing
                Left err -> do
                    liftIO $ removeFile path
                    rollback
                    let msg = printf "Image invalide.\nErreur: %s." (show err)
                    return $ Just $ T.pack msg

    resizeImage picId img picType picExt = do
        let Z :. h  :. w  = I.shape img
            Z :. h' :. w' = picSize picType
            -- Redimensionne l'image sur la dimension la plus petite.
            ratio = min (w % w') (h % h')
            (tmpW, tmpH) =
                (truncate ((w % 1) / ratio), truncate ((h % 1) / ratio))
            tmp = I.resize img I.Bilinear (Z :. tmpH :. tmpW) :: I.RGBDelayed
            -- Coupe l'image sur la partie centrale.
            rect = Rect ((tmpW - w') `div` 2) ((tmpH - h') `div` 2) w' h'
            img' = I.crop tmp rect :: I.RGBImage
        err <- liftIO $ I.save (picPath picId picType picExt) img'
        case err of
            Just msg -> error (show msg)
            Nothing  -> return ()

    picSize PicSmall     = Z :. 75  :. 75
    picSize PicLarge     = Z :. 400 :. 350
    picSize PicWide      = Z :. 300 :. 1170
    picSize PicCatalogue = Z :. 80  :. 255
    picSize PicOriginal  = undefined

    rollback = do conn <- askSqlConn
                  liftIO $ connRollback conn (connPrepare conn)

pictureForm :: Form FileInfo
pictureForm = renderDivs $ fileAFormReq "Fichier de l'image"

getAdminRemovePictureR :: PictureId -> Handler ()
getAdminRemovePictureR picId = do
    redirectIfNotConnected

    prodId <- runDB $ do
        pic <- get404 picId
        removePicture picId (pictureExtension pic)
        return $ pictureProduct pic

    redirect (AdminPicturesR prodId)

removePicture :: PictureId -> Text -> YesodDB App ()
removePicture picId picExt = do
    delete picId
    removeFile' PicOriginal
    removeFile' PicSmall
    removeFile' PicLarge
    removeFile' PicWide
    removeFile' PicCatalogue
  where
    removeFile' picType = liftIO $ removeFile $ picPath picId picType picExt

-- Catalogue -------------------------------------------------------------------

-- | Liste des catégories.
getAdminCategoriesR :: Handler Html
getAdminCategoriesR = do
    redirectIfNotConnected

    (widget, enctype) <- generateFormPost $ categoryForm Nothing

    cats <- runDB listCats

    defaultLayout $ do
        setTitle "Gestion des catégories - Be Chouette"
        $(widgetFile "admin-categories")

-- | Ajout d'une nouvelle catégorie.
postAdminCategoriesR :: Handler Html
postAdminCategoriesR = do
    redirectIfNotConnected

    ((result, widget), enctype) <- runFormPost $ categoryForm Nothing

    err <- case result of
        FormSuccess cat -> do
            m <- runDB $ insertUnique cat
            case m of
                Just _  -> redirect AdminCategoriesR
                Nothing -> return $! Just ("Nom de catégorie existant." :: Text)
        _               -> return Nothing

    cats <- runDB listCats

    defaultLayout $ do
        setTitle "Gestion des catégories - Be Chouette"
        $(widgetFile "admin-categories")

-- | Modifier une catégorie.
getAdminCategoryR :: CategoryId -> Handler Html
getAdminCategoryR catId = do
    redirectIfNotConnected

    cat <- runDB $ get404 catId

    (widget, enctype) <- generateFormPost $ categoryForm (Just cat)

    let err = Nothing
    defaultLayout $ do
        setTitle "Modifier une catégorie - Be Chouette"
        $(widgetFile "admin-category")

postAdminCategoryR :: CategoryId -> Handler Html
postAdminCategoryR catId = do
    redirectIfNotConnected

    cat <- runDB $ get404 catId

    ((result, widget), enctype) <- runFormPost $ categoryForm Nothing

    err <- case result of
        FormSuccess cat' -> do
            m <- runDB $
                -- Teste si une catégorie du même nom existe.
                mCatName <- getBy $ CategoryName $ categoryName cat'
                case mCatName of
                    Just (Entity catNameId _) | catNameId /= catId ->
                        return $! Just ("Nom de catégorie existant." :: Text)
                    _                                              -> do
                        runDB $ update catId cat'
                        return Nothing
        _                ->
            return Nothing

    case err of
        Just _ -> do
            defaultLayout $ do
                setTitle "Modifier une catégorie - Be Chouette"
                $(widgetFile "admin-category")
        Nothing ->
            redirect AdminCategoriesR

getAdminCategoryRemoveR :: CategoryId -> Handler ()
getAdminCategoryRemoveR catId = do
    redirectIfNotConnected

    runDB $ do
        _ <- delete catId
        subCats <- selectList [SubCategoryCategory ==. catId]
                              [Asc SubCategoryName]
        forM_ subCats (delete . entityKey)

    redirect AdminCategoriesR

categoryForm :: Maybe Category -> Form Category
categoryForm cat = renderDivs $ Category
    <$> areq textField "Nom de la catégorie" (categoryName <$> cat)
    <*> areq intField  "Ordre de la catégorie (classées par ordre croissant)"
             (categoryOrder <$> cat)

-- Sous-catégories -------------------------------------------------------------

-- | Ajoute une nouvelle sous-catégorie.
getAdminSubCatNewR :: CategoryId -> Handler Html
getAdminSubCatNewR catId = do
    redirectIfNotConnected

    cat <- runDB $ get404 catId

    let err = Nothing
    (widget, enctype) <- generateFormPost $ subCatForm catId [] Nothing

    defaultLayout $ do
        setTitle "Ajouter une sous-catégorie - Be Chouette"
        $(widgetFile "admin-subcategory-new")

postAdminSubCatNewR :: CategoryId -> Handler Html
postAdminSubCatNewR catId = do
    redirectIfNotConnected

    cat <- runDB $ get404 catId

    ((result, widget), enctype) <- runFormPost $ subCatForm catId [] Nothing

    err <- case result of
        FormSuccess subCat -> do
            m <- runDB $ insertUnique subCat
            case m of
                Just _  -> return $! Just ("Nom de catégorie existant." :: Text)
                Nothing ->  redirect AdminCategoriesR
        _                  -> return Nothing

    defaultLayout $ do
        setTitle "Ajouter une sous-catégorie - Be Chouette"
        $(widgetFile "admin-subcategory-new")

-- | Modifie le nom et l'article principal d'une sous-catégorie.
getAdminSubCatEditR :: SubCategoryId -> Handler Html
getAdminSubCatEditR subCatId = do
    redirectIfNotConnected

    (subCat, prods) <- runDB $
        (,) <$> get404 subCatId
            <*> listSubCatProducts subCatId

    let err   = Nothing
        catId = subCategoryCategory subCat
    (widget, enctype) <- generateFormPost $ subCatForm catId prods (Just subCat)

    defaultLayout $ do
        setTitle "Modifier une sous-catégorie - Be Chouette"
        $(widgetFile "admin-subcategory-edit")

postAdminSubCatEditR :: SubCategoryId -> Handler Html
postAdminSubCatEditR subCatId = do
    redirectIfNotConnected

    (subCat, prods) <- runDB $
        (,) <$> get404 subCatId
            <*> listSubCatProducts subCatId

    let catId = subCategoryCategory subCat
    ((result, widget), enctype) <- runFormPost $ subCatForm catId prods
                                                            (Just subCat)

    err <- case result of
        FormSuccess subCat' -> do
            -- Teste si une catégorie du même nom existe.
            mSubCatName <- getBy $ SubCategoryName $ subCategoryName subCat'
            case mSubCatName of
                Just (Entity subCatNameId _) | subCatNameId /= subCatCatId ->
                    return $! Just ("Nom de catégorie existant." :: Text)
                _                                                          -> do
                    runDB $ update subCatId subCat'
                    redirect AdminCategoriesR
        _                   -> return Nothing

    defaultLayout $ do
        setTitle "Modifier une sous-catégorie - Be Chouette"
        $(widgetFile "admin-subcategory-edit")

subCatForm :: CategoryId -> [Entity Product] -> Maybe SubCategory
           -> Form SubCategory
subCatForm catId prods subCat = renderDivs $ SubCategory catId
    <$> areq textField     "Nom de la sous-catégorie"
             (subCategoryName <$> subCat)
    <*> areq textField     "Description rapide (pour catalogue)"
             (subCategoryShortDesc <$> subCat)
    <*> mainProductField
    <*> areq checkBoxField "Afficher en grand sur la page d'acceuil"
             (subCategoryTop <$> subCat)
  where
    mainProductField | null prods = pure Nothing
                     | otherwise  =
        aopt (selectFieldList (productNames prods))
             "Produit principal (utilisé pour la photo)"
             (subCategoryMainProduct <$> subCat)

-- | Liste les produits d'une sous-catégorie.
getAdminSubCatR :: SubCategoryId -> Handler Html
getAdminSubCatR subCatId = do
    redirectIfNotConnected

    (subCat, prods) <- runDB $
        (, ) <$> get404 subCatId
             <*> listNotSubCatProducts subCatId

    (widget, enctype) <- generateFormPost $ productSelectionForm prods

    defaultLayout $ do
        setTitle "Modifier les produits d'une sous-catégorie - Be Chouette"
        $(widgetFile "admin-subcategory")

-- | Ajoute un produit et liste les produits d'une sous-catégorie.
postAdminSubCatR :: SubCategoryId -> Handler Html
postAdminSubCatR subCatId = do
    redirectIfNotConnected

    (subCat, prods) <- runDB $
        (, ) <$> get404 subCatId
             <*> listNotSubCatProducts subCatId

    ((result, widget), enctype) <- runFormPost $ productSelectionForm prods

    case result of
        FormSuccess prodId -> insertUnique $ SubCategoryProduct subCatId prodId
        _                  -> return ()

    defaultLayout $ do
        setTitle "Modifier les produits d'une sous-catégorie - Be Chouette"
        $(widgetFile "admin-subcategory")

-- | Supprime une sous-catégorie et ses produits.
getAdminSubCatRemoveR :: SubCategoryId -> Handler ()
getAdminSubCatRemoveR subCatId = do
    redirectIfNotConnected

    runDB $ do
        _ <- get404 subCatId
        delete subCatId

        prods <- listSubCatProducts subCatId
        forM_ prods $ delete . entityKey

    redirect AdminCategoriesR

-- | Supprime un produit de la sous catégorie.
getAdminSubCatRemProdR :: SubCategoryId -> SubCategoryProductId -> Handler ()
getAdminSubCatRemProdR subCatId subCatProductId = do
    redirectIfNotConnected

    prods <- runDB $ do
        subCat <- get404 subCatId
        delete subCatProductId

        when (subCategoryMainProduct subCat == Just subCatProductId) $
            update subCatId subCat { subCategoryMainProduct = Nothing }

    redirect (AdminSubCatR subCatId)

listSubCatProducts :: SubCategoryId -> YesodDB App [Entity Product]
listSubCatProducts subCatId = do
    subCatProds <- selectList [SubCategoryProductSubCategory ==. subCatId] []

    selectList [ProductId <-. map subCategoryProductProduct subCatProds]
               [Asc ProductName]

listNotSubCatProducts :: SubCategoryId -> YesodDB App [Entity Product]
listNotSubCatProducts subCatId = do
    subCatProds <- selectList [SubCategoryProductSubCategory ==. subCatId] []

    selectList [ProductId /<-. map subCategoryProductProduct subCatProds]
               [Asc ProductName]

-- Listes de naissance ---------------------------------------------------------

-- | Lister et ajouter des listes de naissance.
getAdminBirthListsR :: Handler Html
getAdminBirthListsR = do
    redirectIfNotConnected

    (widget, enctype) <- generateFormPost $ birthlistForm [] Nothing

    bls <- runDB $ listBirthLists

    let err = Nothing
    defaultLayout $ do
        setTitle "Gérer les listes de naissance - Be Chouette"
        $(widgetFile "admin-birthlists")

postAdminBirthListR :: Handler Html
postAdminBirthListR = do
    redirectIfNotConnected

    ((result, widget), enctype) <- runFormPost $ birthlistForm [] Nothing

    err <- case result of
        FormSuccess bl -> do
            m <- runDB $ insertUnique bl
            case m of
                Just _  -> return $! Just ("Nom de liste existant." :: Text)
                Nothing -> redirect AdminBirthListR
        _               -> return Nothing

    bls <- runDB $ listBirthLists

    defaultLayout $ do
        setTitle "Gérer les listes de naissance - Be Chouette"
        $(widgetFile "admin-birthlists")

-- | Modifie le nom et l'article principal d'une liste de naissance.
getAdminBirthListEditR :: BirthListId -> Handler Html
getAdminBirthListEditR blId = do
    redirectIfNotConnected

    (bl, prods) <- runDB $
        (,) <$> get404 blId
            <*> listBirthListProducts subCatId

    let err   = Nothing
    (widget, enctype) <- generateFormPost $ birthlistForm prods (Just bl)

    defaultLayout $ do
        setTitle "Modifier une liste de naissance - Be Chouette"
        $(widgetFile "admin-birthlist-edit")

postAdminBirthListEditR :: BirthListId -> Handler Html
postAdminBirthListEditR blId = do
    redirectIfNotConnected

    (bl, prods) <- runDB $
        (,) <$> get404 blId
            <*> listBirthListProducts blId

    ((result, widget), enctype) <- runFormPost $ birthlistForm prods (Just bl)

    err <- case result of
        FormSuccess bl' -> do
            -- Teste si une liste du même nom existe.
            mBlName <- getBy $ BirthListName $ birthListName bl'
            case mBlName of
                Just (Entity blNameId _) | blNameId /= blId ->
                    return $! Just ("Nom de liste existant." :: Text)
                _                                           -> do
                    runDB $ update blId bl'
                    redirect AdminBirthListsR
        _               -> return Nothing

    defaultLayout $ do
        setTitle "Modifier une liste de naissance - Be Chouette"
        $(widgetFile "admin-birthlist-edit")

birthlistForm :: [Entity Product] -> Maybe BirthList -> Form BirthList
birthlistForm prods bl = renderDivs $ BirthList
    <$> areq textField "Nom de la liste de naissance" (birthListyName <$> bl)
  where
    mainProductField | null prods = pure Nothing
                     | otherwise  =
        aopt (selectFieldList (productNames prods))
             "Produit principal (utilisé pour la photo)"
             (birthListMainProduct <$> bl)

-- | Liste les produits d'une liste de naissance.
getAdminBirthListR :: BirthListId -> Handler Html
getAdminBirthListR blId = do
    redirectIfNotConnected

    (bl, prods) <- runDB $
        (, ) <$> get404 blId
             <*> listNotSubCatProducts subCatId

    (widget, enctype) <- generateFormPost $ productSelectionForm prods

    defaultLayout $ do
        setTitle "Modifier les produits d'une liste de naissance - Be Chouette"
        $(widgetFile "admin-birthlist")

-- | Ajoute un produit et liste les produits d'une sous-catégorie.
postAdminSubCatR :: SubCategoryId -> Handler Html
postAdminSubCatR subCatId = do
    redirectIfNotConnected

    (subCat, prods) <- runDB $
        (, ) <$> get404 subCatId
             <*> listNotSubCatProducts subCatId

    ((result, widget), enctype) <- runFormPost $ productSelectionForm prods

    case result of
        FormSuccess prodId -> insertUnique $ BirthListProduct blId prodId False
        _                  -> return ()

    defaultLayout $ do
        setTitle "Modifier les produits d'une liste de naissance - Be Chouette"
        $(widgetFile "admin-birthlist")

-- | Supprime une liste de naissance et ses produits.
getAdminBirthListRemoveR :: BirthListId -> Handler ()
getAdminBirthListRemoveR blId = do
    redirectIfNotConnected

    runDB $ do
        _ <- get404 blId
        delete blId

        prods <- listBirthListProducts blId
        forM_ prods $ delete . entityKey

    redirect AdminBirthListsR

-- | Supprime un produit de la liste de naissance.
getAdminSubCatRemProdR :: BirthListId -> BirthListProductId -> Handler ()
getAdminSubCatRemProdR blId blProductId = do
    redirectIfNotConnected

    prods <- runDB $ do
        bl <- get404 blId
        delete blProductId

        when (birthListMainProduct bl == Just blProductId) $
            update blId bl { birthListMainProduct = Nothing }

    redirect (AdminBirthListR blId)

-- | Réserve un produit de la liste de naissance.
getAdminSubCatRemProdR :: BirthListId -> BirthListProductId -> Bool
                       -> Handler ()
getAdminSubCatRemProdR blId blProductId reserve = do
    redirectIfNotConnected

    prods <- runDB $ do
        bl     <- get404 blId
        blProd <- get404 blProductId

        update blProductId { birthListProductReserved = reserve }

    redirect (AdminBirthListR blId)

listBirthListProducts :: SubCategoryId -> YesodDB App [Entity Product]
listBirthListProducts blId = do
    blProds <- selectList [BirthListId ==. blId] []

    selectList [ProductId <-. map birthListProductProduct blProds]
               [Asc ProductName]

listNotBirthListProducts :: BirthListId -> YesodDB App [Entity Product]
listNotBirthListProducts blId = do
    blProds <- selectList [BirthListId ==. blId] []

    selectList [ProductId /<-. map birthListProductProduct blProds]
               [Asc ProductName]

-- -----------------------------------------------------------------------------

productNames :: [Entity Product] -> [(Text, ProductId)]
productNames = map (\(Entity prodId prod) -> (productName prod, prodId))

productSelectionForm :: [Entity Product] -> Form ProductId
productSelectionForm prods = renderDivs $
    areq (selectFieldList (productNames prods)) "Produit" Nothing

redirectIfNotConnected :: Handler ()
redirectIfNotConnected = do
    v <- lookupSession sessionKey
    case v of
        Just _  -> return ()
        Nothing -> redirect AdminLoginR
