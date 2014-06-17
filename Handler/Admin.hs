{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Handler.Admin (
      getAdminLoginR, postAdminLoginR

    , getAdminProductsR, postAdminProductsR
    , getAdminProductR, postAdminProductR, getAdminProductRemoveR

    , getAdminPicturesR, postAdminPicturesR, getAdminPictureRemoveR

    , getAdminCategoriesR, postAdminCategoriesR
    , getAdminCategoryR, postAdminCategoryR, getAdminCategoryRemoveR

    , getAdminSubCatNewR, postAdminSubCatNewR
    , getAdminSubCatEditR, postAdminSubCatEditR
    , getAdminSubCatR, postAdminSubCatR
    , getAdminSubCatRemoveR, getAdminSubCatRemProdR

    , getAdminBirthListsR, postAdminBirthListsR
    , getAdminBirthListEditR, postAdminBirthListEditR
    , getAdminBirthListR, postAdminBirthListR
    , getAdminBirthListRemoveR, getAdminBirthListRemProdR
    , getAdminBirthListReserveProdR
    ) where

import Import
import Prelude (tail)
import Control.Applicative
import Control.Monad
import Data.Ratio
import Database.Persist.Sql (askSqlConn, connPrepare, connRollback)
import qualified Data.Text as T
import System.Directory
import System.FilePath (takeExtension)
import qualified Vision.Image as I
import Vision.Primitive (Z (..), (:.) (..), Rect (..))
import Text.Printf
import Yesod.Form.Bootstrap3

import Handler.Home (listCats)
import Handler.Catalog (listSubCatProducts)
import Handler.BirthList (listBirthLists, listBirthListProducts)

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
            redirect AdminProductsR
        _ -> do
            let err = Just "Mot de passe invalide." :: Maybe Text
            defaultLayout $ do
                setTitle "Connexion à l'administration - Be Chouette"
                $(widgetFile "admin-login")

loginForm :: Form Text
loginForm = renderBootstrap3 BootstrapBasicForm $
       areq passwordField "Mot de passe" Nothing
    <* bootstrapSubmit' "Se connecter"

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

    (widget, enctype) <- generateFormPost (prodForm Nothing
                                                    "Ajouter le produit")

    prods <- runDB $ selectList [] [Asc ProductName]

    let err = Nothing :: Maybe Text
    defaultLayout $ do
        setTitle "Gestion des produits - Be Chouette"
        $(widgetFile "admin-products")

-- | Nouveau produit.
postAdminProductsR :: Handler Html
postAdminProductsR = do
    redirectIfNotConnected

    ((result, widget), enctype) <- runFormPost (prodForm Nothing
                                                         "Ajouter le produit")

    prods <- runDB $ selectList [] [Asc ProductName]

    err <- case result of
        FormSuccess prod -> do
            m <- runDB $ insertUnique prod
            case m of
                Just _  -> redirect AdminProductsR
                Nothing -> return $! Just ("Nom de produit existant." :: Text)
        _               -> return Nothing

    defaultLayout $ do
        setTitle "Ajouter un nouveau produit - Be Chouette"
        $(widgetFile "admin-products")

getAdminProductR :: ProductId -> Handler Html
getAdminProductR prodId = do
    redirectIfNotConnected

    prod <- runDB $ get404 prodId

    (widget, enctype) <- generateFormPost (prodForm (Just prod)
                                                    "Modifier le produit")

    defaultLayout $ do
        setTitle "Modifier un produit - Be Chouette"
        $(widgetFile "admin-product")

postAdminProductR :: ProductId -> Handler Html
postAdminProductR prodId = do
    redirectIfNotConnected

    prod <- runDB $ get404 prodId

    ((result, widget), enctype) <- runFormPost (prodForm (Just prod)
                                                         "Modifier le produit")

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

    subCatProds <- selectList [SubCategoryProductProduct ==. prodId] []
    forM_ subCatProds $ delete . entityKey

    subCats <- selectList [SubCategoryMainProduct ==. Just prodId] []
    forM_ subCats $ flip update [SubCategoryMainProduct =. Nothing] . entityKey

    blProds <- selectList [BirthListProductProduct ==. prodId] []
    forM_ blProds $ delete . entityKey

    bls <- selectList [BirthListMainProduct ==. Just prodId] []
    forM_ bls $ flip update [BirthListMainProduct =. Nothing] . entityKey

prodForm :: Maybe Product -> BootstrapSubmit Text -> Form Product
prodForm prod submitMsg =
    renderBootstrap3 BootstrapBasicForm $ Product
        <$> areq textField (bfs' "Nom du produit") (productName <$> prod)
        <*> aopt textField
                 (bfs' "Référence du produit (facultatif)")
                 (productRef <$> prod)
        <*> areq textField
                 (bfs' "Description rapide (pour catalogue et Facebook)")
                 (productShortDesc <$> prod)
        <*> areq textareaField
                 (bfs' "Description complète (pour la fiche produit)")
                 (productDesc <$> prod)
        <*> areq textareaField (bfs' "Détails (tailles, couleurs, lavage, ...)")
                (productDetails <$> prod)
        <*> aopt doubleField
                (bfs' "Prix (facultatif, '.' pour séparer les décimales)")
                (productPrice <$> prod)
        <*> areq checkBoxField (bfs' "Disponible à la vente")
                 (productAvailable <$> prod)
        <* bootstrapSubmit' submitMsg

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

    picSize PicSmall     = Z :. 55  :. 55
    picSize PicLarge     = Z :. 269 :. 235
    picSize PicWide      = Z :. 300 :. 1170
    picSize PicCatalogue = Z :. 80  :. 255
    picSize PicOriginal  = undefined

    rollback = do conn <- askSqlConn
                  liftIO $ connRollback conn (connPrepare conn)

pictureForm :: Form FileInfo
pictureForm = renderBootstrap3 BootstrapBasicForm $
       areq fileField (bfs' "Fichier de l'image") Nothing
    <* bootstrapSubmit' "Ajouter l'image"

getAdminPictureRemoveR :: PictureId -> Handler ()
getAdminPictureRemoveR picId = do
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

    (widget, enctype) <-
        generateFormPost $ categoryForm Nothing "Créer la catégorie"

    cats <- runDB listCats

    let err = Nothing :: Maybe Text
    defaultLayout $ do
        setTitle "Gestion des catégories - Be Chouette"
        $(widgetFile "admin-categories")

-- | Ajout d'une nouvelle catégorie.
postAdminCategoriesR :: Handler Html
postAdminCategoriesR = do
    redirectIfNotConnected

    ((result, widget), enctype) <-
        runFormPost $ categoryForm Nothing "Créer la catégorie"

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

    (widget, enctype) <-
        generateFormPost $ categoryForm (Just cat) "Modifier la catégorie"

    let err = Nothing :: Maybe Text
    defaultLayout $ do
        setTitle "Modifier une catégorie - Be Chouette"
        $(widgetFile "admin-category")

postAdminCategoryR :: CategoryId -> Handler Html
postAdminCategoryR catId = do
    redirectIfNotConnected

    cat <- runDB $ get404 catId

    ((result, widget), enctype) <- 
        runFormPost $ categoryForm (Just cat) "Modifier la catégorie"

    err <- case result of
        FormSuccess cat' -> do
            -- Teste si une catégorie du même nom existe.
            mCatName <- runDB $ getBy $ UniqueCategoryName $ categoryName cat'
            case mCatName of
                Just (Entity catNameId _) | catNameId /= catId ->
                    return $! Just ("Nom de catégorie existant." :: Text)
                _                                              -> do
                    runDB $ replace catId cat'
                    redirect AdminCategoriesR
        _                ->
            return Nothing

    defaultLayout $ do
        setTitle "Modifier une catégorie - Be Chouette"
        $(widgetFile "admin-category")

getAdminCategoryRemoveR :: CategoryId -> Handler ()
getAdminCategoryRemoveR catId = do
    redirectIfNotConnected

    runDB $ do
        _ <- delete catId
        subCats <- selectList [SubCategoryCategory ==. catId]
                              [Asc SubCategoryName]
        forM_ subCats $ \(Entity subCatId _) -> do
            prods <- listSubCatProducts subCatId
            forM_ prods $ delete . entityKey . fst

            delete subCatId

    redirect AdminCategoriesR

categoryForm :: Maybe Category -> BootstrapSubmit Text -> Form Category
categoryForm cat submitMsg = renderBootstrap3 BootstrapBasicForm $ Category
    <$> areq textField (bfs' "Nom de la catégorie") (categoryName <$> cat)
    <*> areq intField
             (bfs' "Ordre de la catégorie (classées par ordre croissant)")
             (categoryOrder <$> cat)
    <*  bootstrapSubmit' submitMsg

-- Sous-catégories -------------------------------------------------------------

-- | Ajoute une nouvelle sous-catégorie.
getAdminSubCatNewR :: CategoryId -> Handler Html
getAdminSubCatNewR catId = do
    redirectIfNotConnected

    cat <- runDB $ get404 catId

    let err = Nothing :: Maybe Text
    (widget, enctype) <-
        generateFormPost $ subCatForm catId [] Nothing "Créer la sous-catégorie"

    defaultLayout $ do
        setTitle "Ajouter une sous-catégorie - Be Chouette"
        $(widgetFile "admin-subcategory-new")

postAdminSubCatNewR :: CategoryId -> Handler Html
postAdminSubCatNewR catId = do
    redirectIfNotConnected

    cat <- runDB $ get404 catId

    ((result, widget), enctype) <-
        runFormPost $ subCatForm catId [] Nothing "Créer la sous-catégorie"

    err <- case result of
        FormSuccess subCat -> do
            m <- runDB $ insertUnique subCat
            case m of
                Nothing -> return $! Just ("Nom de catégorie existant." :: Text)
                Just _  -> redirect AdminCategoriesR
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
            <*> (map snd <$> listSubCatProducts subCatId)

    let err   = Nothing :: Maybe Text
        catId = subCategoryCategory subCat
    (widget, enctype) <-
        generateFormPost $ subCatForm catId prods (Just subCat)
                                      "Modifier la sous-catégorie"

    defaultLayout $ do
        setTitle "Modifier une sous-catégorie - Be Chouette"
        $(widgetFile "admin-subcategory-edit")

postAdminSubCatEditR :: SubCategoryId -> Handler Html
postAdminSubCatEditR subCatId = do
    redirectIfNotConnected

    (subCat, prods) <- runDB $
        (,) <$> get404 subCatId
            <*> (map snd <$> listSubCatProducts subCatId)

    let catId = subCategoryCategory subCat
    ((result, widget), enctype) <-
        runFormPost $ subCatForm catId prods (Just subCat)
                                 "Modifier la sous-catégorie"

    err <- case result of
        FormSuccess subCat' -> do
            -- Teste si une catégorie du même nom existe.
            let subCatName' = subCategoryName subCat'
            mSubCatName <- runDB $ getBy $ UniqueSubCategoryName catId
                                                                 subCatName'
            case mSubCatName of
                Just (Entity subCatNameId _) | subCatNameId /= subCatId ->
                    return $! Just ("Nom de catégorie existant." :: Text)
                _                                                          -> do
                    runDB $ replace subCatId subCat'
                    redirect AdminCategoriesR
        _                   -> return Nothing

    defaultLayout $ do
        setTitle "Modifier une sous-catégorie - Be Chouette"
        $(widgetFile "admin-subcategory-edit")

subCatForm :: CategoryId -> [Entity Product] -> Maybe SubCategory
           -> BootstrapSubmit Text -> Form SubCategory
subCatForm catId prods subCat submitMsg =
    renderBootstrap3 BootstrapBasicForm $ SubCategory catId
        <$> areq textField     (bfs' "Nom de la sous-catégorie")
                 (subCategoryName <$> subCat)
        <*> areq textField     (bfs' "Description rapide (pour catalogue)")
                 (subCategoryShortDesc <$> subCat)
        <*> mainProductField
        <*> areq checkBoxField (bfs' "Afficher en grand sur la page d'acceuil")
                 (subCategoryTop <$> subCat)
        <* bootstrapSubmit' submitMsg
  where
    mainProductField | null prods = pure Nothing
                     | otherwise  =
        aopt (selectFieldList (productNames prods))
             (bfs' "Produit principal (utilisé pour la photo)")
             (subCategoryMainProduct <$> subCat)

-- | Liste les produits d'une sous-catégorie.
getAdminSubCatR :: SubCategoryId -> Handler Html
getAdminSubCatR subCatId = do
    redirectIfNotConnected

    (subCat, notProds, prods) <- runDB $
        (,,) <$> get404 subCatId
             <*> listNotSubCatProducts subCatId
             <*> listSubCatProducts    subCatId

    (widget, enctype) <- generateFormPost $ productSelectionForm notProds

    defaultLayout $ do
        setTitle "Modifier les produits d'une sous-catégorie - Be Chouette"
        $(widgetFile "admin-subcategory")

-- | Ajoute un produit et liste les produits d'une sous-catégorie.
postAdminSubCatR :: SubCategoryId -> Handler Html
postAdminSubCatR subCatId = do
    redirectIfNotConnected

    (subCat, notProds, prods) <- runDB $
        (,,) <$> get404 subCatId
             <*> listNotSubCatProducts subCatId
             <*> listSubCatProducts    subCatId

    ((result, widget), enctype) <- runFormPost $ productSelectionForm notProds

    case result of
        FormSuccess prodId -> do
            _ <- runDB $ insertUnique $ SubCategoryProduct subCatId prodId
            redirect (AdminSubCatR subCatId)
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
        forM_ prods $ delete . entityKey . fst

    redirect AdminCategoriesR

-- | Supprime un produit de la sous catégorie.
getAdminSubCatRemProdR :: SubCategoryId -> SubCategoryProductId -> Handler ()
getAdminSubCatRemProdR subCatId subCatProdId = do
    redirectIfNotConnected

    runDB $ do
        subCat     <- get404 subCatId
        subCatProd <- get404 subCatProdId
        delete subCatProdId

        let prodId = subCategoryProductProduct subCatProd
        when (subCategoryMainProduct subCat == Just prodId) $
            update subCatId [SubCategoryMainProduct =. Nothing]

    redirect (AdminSubCatR subCatId)

listNotSubCatProducts :: SubCategoryId -> YesodDB App [Entity Product]
listNotSubCatProducts subCatId = do
    subCatProds <- selectList [SubCategoryProductSubCategory ==. subCatId] []

    selectList [ProductId /<-. map (subCategoryProductProduct . entityVal) 
                                   subCatProds]
               [Asc ProductName]

-- Listes de naissance ---------------------------------------------------------

-- | Lister et ajouter des listes de naissance.
getAdminBirthListsR :: Handler Html
getAdminBirthListsR = do
    redirectIfNotConnected

    (widget, enctype) <-
        generateFormPost $ birthlistForm [] Nothing "Créer la liste"

    bls <- runDB $ listBirthLists

    let err = Nothing :: Maybe Text
    defaultLayout $ do
        setTitle "Gérer les listes de naissance - Be Chouette"
        $(widgetFile "admin-birthlists")

postAdminBirthListsR :: Handler Html
postAdminBirthListsR = do
    redirectIfNotConnected

    ((result, widget), enctype) <-
        runFormPost $ birthlistForm [] Nothing "Créer la liste"

    err <- case result of
        FormSuccess bl -> do
            m <- runDB $ insertUnique bl
            case m of
                Nothing -> return $! Just ("Nom de liste existant." :: Text)
                Just _  -> redirect AdminBirthListsR
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
            <*> (map snd <$> listBirthListProducts blId)

    let err   = Nothing :: Maybe Text
    (widget, enctype) <- 
        generateFormPost $ birthlistForm prods (Just bl)  "Modifier la liste"

    defaultLayout $ do
        setTitle "Modifier une liste de naissance - Be Chouette"
        $(widgetFile "admin-birthlist-edit")

postAdminBirthListEditR :: BirthListId -> Handler Html
postAdminBirthListEditR blId = do
    redirectIfNotConnected

    (bl, prods) <- runDB $
        (,) <$> get404 blId
            <*> (map snd <$> listBirthListProducts blId)

    ((result, widget), enctype) <-
        runFormPost $ birthlistForm prods (Just bl) "Modifier la liste"

    err <- case result of
        FormSuccess bl' -> do
            -- Teste si une liste du même nom existe.
            mBlName <- runDB $ getBy $
                UniqueBirthListName (birthListName bl') (birthListParents bl')
            case mBlName of
                Just (Entity blNameId _) | blNameId /= blId ->
                    return $! Just ("Nom de liste existant." :: Text)
                _                                           -> do
                    runDB $ replace blId bl'
                    redirect AdminBirthListsR
        _               -> return Nothing

    defaultLayout $ do
        setTitle "Modifier une liste de naissance - Be Chouette"
        $(widgetFile "admin-birthlist-edit")

birthlistForm :: [Entity Product] -> Maybe BirthList -> BootstrapSubmit Text
              -> Form BirthList
birthlistForm prods bl submitMsg =
    renderBootstrap3 BootstrapBasicForm $ BirthList
        <$> areq textField (bfs' "Nom de l'enfant") (birthListName <$> bl)
        <*> areq textField (bfs' "Nom des parents") (birthListParents <$> bl)
        <*> mainProductField
        <*  bootstrapSubmit' submitMsg
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

    (bl, notProds, prods) <- runDB $
        (,,) <$> get404 blId
             <*> listNotBirthListProducts blId
             <*> listBirthListProducts    blId

    (widget, enctype) <- generateFormPost $ productSelectionForm notProds

    defaultLayout $ do
        setTitle "Modifier les produits d'une liste de naissance - Be Chouette"
        $(widgetFile "admin-birthlist")

-- | Ajoute un produit et liste les produits d'une liste de naissance.
postAdminBirthListR :: BirthListId -> Handler Html
postAdminBirthListR blId = do
    redirectIfNotConnected

    (bl, notProds, prods) <- runDB $
        (,,) <$> get404 blId
             <*> listNotBirthListProducts blId
             <*> listBirthListProducts    blId

    ((result, widget), enctype) <- runFormPost $ productSelectionForm notProds

    case result of
        FormSuccess prodId -> do
            _ <- runDB $ insertUnique $ BirthListProduct blId prodId False
            redirect (AdminBirthListR blId)
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

        blProds <- listBirthListProducts blId
        forM_ blProds $ delete . entityKey . fst

    redirect AdminBirthListsR

-- | Supprime un produit de la liste de naissance.
getAdminBirthListRemProdR :: BirthListId -> BirthListProductId -> Handler ()
getAdminBirthListRemProdR blId blProdId = do
    redirectIfNotConnected

    runDB $ do
        bl     <- get404 blId
        blProd <- get404 blProdId
        delete blProdId

        let prodId = birthListProductProduct blProd
        when (birthListMainProduct bl == Just prodId) $
            update blId [BirthListMainProduct =. Nothing]

    redirect (AdminBirthListR blId)

-- | Réserve un produit de la liste de naissance.
getAdminBirthListReserveProdR :: BirthListId -> BirthListProductId -> Int
                              -> Handler ()
getAdminBirthListReserveProdR blId blProdId reserve = do
    redirectIfNotConnected

    runDB $ do
        _ <- get404 blId
        _ <- get404 blProdId

        update blProdId [BirthListProductReserved =. toEnum reserve]

    redirect (AdminBirthListR blId)

listNotBirthListProducts :: BirthListId -> YesodDB App [Entity Product]
listNotBirthListProducts blId = do
    blProds <- selectList [BirthListProductBirthList ==. blId] []

    selectList [ProductId /<-. map (birthListProductProduct . entityVal) 
                                   blProds]
               [Asc ProductName]

-- -----------------------------------------------------------------------------

productNames :: [Entity Product] -> [(Text, ProductId)]
productNames = map (\(Entity prodId prod) -> (productName prod, prodId))

productSelectionForm :: [Entity Product] -> Form ProductId
productSelectionForm prods = renderBootstrap3 BootstrapBasicForm $
       areq (selectFieldList (productNames prods)) (bfs' "Produit") Nothing
    <* bootstrapSubmit' "Ajouter le produit"

redirectIfNotConnected :: Handler ()
redirectIfNotConnected = do
    v <- lookupSession sessionKey
    case v of
        Just _  -> return ()
        Nothing -> redirect AdminLoginR
