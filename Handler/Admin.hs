{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Handler.Admin (
      getAdminR, postAdminR, getAdminLoginR, postAdminLoginR
    , getAdminEditCatR, postAdminEditCatR, getAdminRemoveCatR
    , getAdminNewProdR, postAdminNewProdR, getAdminEditProdR, postAdminEditProdR
    , getAdminRemoveProdR, getAdminPicturesR, postAdminPicturesR
    , getAdminRemovePictureR
    ) where

import Import
import Prelude (tail)
import qualified Control.Exception as E
import Control.Monad
import Data.Ratio
import Database.Persist.GenericSql (rollback)
import qualified Data.Text as T
import System.Directory
import System.FilePath (takeExtension)
import qualified Vision.Image as I
import qualified Vision.Primitive as I

import Handler.Home (listProducts)

sessionKey :: Text
sessionKey = "CONNECTED"

getAdminR :: Handler RepHtml
getAdminR = do
    redirectIfNotConnected

    (widget, enctype) <- generateFormPost (categoryForm Nothing)

    cats <- runDB listProducts

    let err = Nothing :: Maybe Text
    defaultLayout $ do
        setTitle "Administration - Be Chouette"
        $(widgetFile "admin")

-- | Ajout d'une nouvelle catégorie.
postAdminR :: Handler RepHtml
postAdminR = do
    redirectIfNotConnected

    ((result, widget), enctype) <- runFormPost $ categoryForm Nothing

    err <- case result of
        FormSuccess cat -> do
            m <- runDB $ insertUnique cat
            case m of
                Just _  -> redirect AdminR
                Nothing -> return $! Just ("Nom de catégorie existant." :: Text)
        _ -> 
            return Nothing

    cats <- runDB listProducts

    defaultLayout $ do
        setTitle "Administration - Be Chouette"
        $(widgetFile "admin")

getAdminEditCatR :: CategoryId -> Handler RepHtml
getAdminEditCatR catId = do
    redirectIfNotConnected

    cat <- runDB $ get404 catId

    (widget, enctype) <- generateFormPost $ categoryForm (Just cat)

    defaultLayout $ do
        setTitle "Modifier une catégorie - Be Chouette"
        $(widgetFile "editcat")

postAdminEditCatR :: CategoryId -> Handler RepHtml
postAdminEditCatR catId = do
    redirectIfNotConnected

    cat <- runDB $ get404 catId

    ((result, widget), enctype) <- runFormPost $ categoryForm (Just cat)

    case result of
        FormSuccess newCat -> do
            _ <- runDB $ replace catId newCat
            redirect AdminR
        _ -> do
            defaultLayout $ do
                setTitle "Modifier une catégorie - Be Chouette"
                $(widgetFile "editcat")

getAdminRemoveCatR :: CategoryId -> Handler ()
getAdminRemoveCatR catId = do
    redirectIfNotConnected

    runDB $ do
        _ <- delete catId
        prods <- selectList [ProductCategory ==. catId] []
        forM_ prods (removeProduct . entityKey)

    redirect AdminR

categoryForm :: Maybe Category -> Form Category
categoryForm cat = renderDivs $ Category
    <$> areq textField "Nom de la catégorie" (categoryName <$> cat)
    <*> areq intField  "Ordre de la catégorie (classées par ordre croissant)"
                      (categoryOrder <$> cat)

-- -----------------------------------------------------------------------------

getAdminLoginR :: Handler RepHtml
getAdminLoginR = do
    (widget, enctype) <- generateFormPost loginForm

    let err = Nothing :: Maybe Text
    defaultLayout $ do
        setTitle "Connexion à l'administration - Be Chouette"
        $(widgetFile "login")

postAdminLoginR :: Handler RepHtml
postAdminLoginR = do
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
                $(widgetFile "login")

loginForm :: Form Text
loginForm = renderDivs $ areq passwordField "Mot de passe" Nothing

-- -----------------------------------------------------------------------------

getAdminNewProdR :: CategoryId -> Handler RepHtml
getAdminNewProdR catId = do
    redirectIfNotConnected

    cat <- runDB $ get404 catId

    (widget, enctype) <- generateFormPost (prodForm catId Nothing)

    defaultLayout $ do
        setTitle "Ajouter un nouveau produit - Be Chouette"
        $(widgetFile "newprod")

postAdminNewProdR :: CategoryId -> Handler RepHtml
postAdminNewProdR catId = do
    redirectIfNotConnected

    ((result, widget), enctype) <- runFormPost (prodForm catId Nothing)

    case result of
        FormSuccess prod -> do
            _ <- runDB $ get404 catId >> insert prod
            redirect AdminR
        _ -> do
            cat <- runDB $ get404 catId
            defaultLayout $ do
                setTitle "Ajouter un nouveau produit - Be Chouette"
                $(widgetFile "newprod")

getAdminEditProdR :: ProductId -> Handler RepHtml
getAdminEditProdR prodId = do
    redirectIfNotConnected

    prod <- runDB $ get404 prodId
    let catId = productCategory prod

    (widget, enctype) <- generateFormPost (prodForm catId (Just prod))

    defaultLayout $ do
        setTitle "Modifier un produit - Be Chouette"
        $(widgetFile "editprod")

postAdminEditProdR :: ProductId -> Handler RepHtml
postAdminEditProdR prodId = do
    redirectIfNotConnected

    prod <- runDB $ get404 prodId
    let catId = productCategory prod

    ((result, widget), enctype) <- runFormPost (prodForm catId (Just prod))

    case result of
        FormSuccess newProd -> do
            _ <- runDB $ replace prodId newProd
            redirect AdminR
        _ -> do
            defaultLayout $ do
                setTitle "Modifier un produit - Be Chouette"
                $(widgetFile "editprod")

getAdminRemoveProdR :: ProductId -> Handler ()
getAdminRemoveProdR prodId = do
    redirectIfNotConnected

    runDB $ removeProduct prodId
    redirect AdminR

prodForm :: CategoryId -> Maybe Product -> Form Product
prodForm catId prod = renderDivs $
    Product catId <$> areq textField "Nom du produit" (productName <$> prod)
                  <*> aopt textField "Référence du produit (facultatif)"
                           (productRef <$> prod)
                  <*> areq textField "Description rapide (catalogue et Facebook)"
                           (productShortDesc <$> prod)
                  <*> areq textareaField "Description complète (fiche produit)"
                           (productDesc <$> prod)
                  <*> areq textareaField "Détails (tailles, couleurs, lavage, ...)"
                           (productDetails <$> prod)
                  <*> aopt doubleField
                           "Prix (facultatif, '.' pour séparer les décimales)"
                           (productPrice <$> prod)
                  <*> areq checkBoxField "Disponible à la vente"
                           (productAvailable <$> prod)
                  <*> areq checkBoxField "Afficher à la une de l'accueil"
                           (productTop <$> prod)

-- | Supprime les produits et ses dépendances.
removeProduct :: ProductId -> YesodDB sub App ()
removeProduct prodId = do
    delete prodId

    pics <- selectList [PictureProduct ==. prodId] []
    forM_ pics $ \(Entity picId pic) -> do
        removePicture picId (pictureExtension pic)

-- -----------------------------------------------------------------------------

getAdminPicturesR :: ProductId -> Handler RepHtml
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
        $(widgetFile "pictures")

postAdminPicturesR :: ProductId -> Handler RepHtml
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
        $(widgetFile "pictures")
  where
    processImage info = do
        let picExt = T.pack $ tail $ takeExtension $ T.unpack $ fileName info
        runDB $ do
            picId <- insert (Picture prodId picExt)

            let path = picPath picId PicOriginal picExt
            liftIO $ fileMove info path

            mImg <- liftIO $ E.try $ (I.load path :: IO I.RGBImage)
            case mImg of
                Right img -> do
                    resizeImage picId img PicCatalogue picExt
                    resizeImage picId img PicSmall     picExt
                    resizeImage picId img PicLarge     picExt
                    resizeImage picId img PicWide      picExt
                    return Nothing
                Left (_ :: E.SomeException) -> do
                    liftIO $ removeFile path
                    rollback
                    return $ Just ("Image invalide." :: Text)

    resizeImage picId img picType picExt = do
        let I.Size w h = I.getSize img
            I.Size w' h' = picSize picType
            -- Redimensionne l'image sur la dimension la plus petite.
            ratio = min (w % w') (h % h')
            (tmpW, tmpH) =
                (truncate ((w % 1) / ratio), truncate ((h % 1) / ratio))
            tmp = I.resize I.Bilinear img (I.Size tmpW tmpH)
            -- Coupe l'image sur la partie centrale.
            rect = I.Rect ((tmpW - w') `div` 2) ((tmpH - h') `div` 2) w' h'
            img' = I.crop tmp rect
        liftIO $ I.save img' (picPath picId picType picExt)

    picSize PicSmall     = I.Size 75 75
    picSize PicLarge     = I.Size 350 400
    picSize PicWide      = I.Size 950 300
    picSize PicCatalogue = I.Size 300 95
    picSize PicOriginal  = undefined

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

removePicture :: PictureId -> Text -> YesodDB sub App ()
removePicture picId picExt = do
    delete picId
    removeFile' PicOriginal
    removeFile' PicSmall
    removeFile' PicLarge
    removeFile' PicWide
    removeFile' PicCatalogue
  where
    removeFile' picType = liftIO $ removeFile $ picPath picId picType picExt

-- -----------------------------------------------------------------------------

redirectIfNotConnected :: Handler ()
redirectIfNotConnected = do
    v <- lookupSession sessionKey
    case v of
        Just _  -> return ()
        Nothing -> redirect AdminLoginR
