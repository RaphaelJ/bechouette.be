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

sessionKey :: Text
sessionKey = "CONNECTED"

getAdminR :: Handler Html
getAdminR = do
    redirectIfNotConnected

    (widget, enctype) <- generateFormPost (categoryForm Nothing)

    cats <- runDB listProducts

    let err = Nothing :: Maybe Text
    defaultLayout $ do
        setTitle "Administration - Be Chouette"
        $(widgetFile "admin")

-- | Ajout d'une nouvelle catégorie.
postAdminR :: Handler Html
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

getAdminEditCatR :: CategoryId -> Handler Html
getAdminEditCatR catId = do
    redirectIfNotConnected

    cat <- runDB $ get404 catId

    (widget, enctype) <- generateFormPost $ categoryForm (Just cat)

    defaultLayout $ do
        setTitle "Modifier une catégorie - Be Chouette"
        $(widgetFile "editcat")

postAdminEditCatR :: CategoryId -> Handler Html
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

getAdminLoginR :: Handler Html
getAdminLoginR = do
    (widget, enctype) <- generateFormPost loginForm

    let err = Nothing :: Maybe Text
    defaultLayout $ do
        setTitle "Connexion à l'administration - Be Chouette"
        $(widgetFile "login")

postAdminLoginR :: Handler Html
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

getAdminNewProdR :: CategoryId -> Handler Html
getAdminNewProdR catId = do
    redirectIfNotConnected

    cat <- runDB $ get404 catId

    (widget, enctype) <- generateFormPost (prodForm (Just catId) Nothing)

    defaultLayout $ do
        setTitle "Ajouter un nouveau produit - Be Chouette"
        $(widgetFile "newprod")

postAdminNewProdR :: CategoryId -> Handler Html
postAdminNewProdR catId = do
    redirectIfNotConnected

    ((result, widget), enctype) <- runFormPost (prodForm (Just catId) Nothing)

    case result of
        FormSuccess prod -> do
            _ <- runDB $ get404 catId >> insert prod
            redirect AdminR
        _ -> do
            cat <- runDB $ get404 catId
            defaultLayout $ do
                setTitle "Ajouter un nouveau produit - Be Chouette"
                $(widgetFile "newprod")

getAdminEditProdR :: ProductId -> Handler Html
getAdminEditProdR prodId = do
    redirectIfNotConnected

    prod <- runDB $ get404 prodId

    (widget, enctype) <- generateFormPost (prodForm Nothing (Just prod))

    defaultLayout $ do
        setTitle "Modifier un produit - Be Chouette"
        $(widgetFile "editprod")

postAdminEditProdR :: ProductId -> Handler Html
postAdminEditProdR prodId = do
    redirectIfNotConnected

    prod <- runDB $ get404 prodId

    ((result, widget), enctype) <- runFormPost (prodForm Nothing (Just prod))

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

prodForm :: Maybe CategoryId -> Maybe Product -> Form Product
prodForm mCatId prod html = do
    catFields <- lift $ runDB $ do
        cats <- selectList [] [Asc CategoryName]
        forM cats $ \(Entity catId cat) -> do
            return (categoryName cat, catId)

    flip renderDivs html $ Product
        <$> areq (selectFieldList catFields) "Catégorie du produit"
                 ((productCategory <$> prod) <|> mCatId)
        <*> areq textField "Nom du produit" (productName <$> prod)
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
        <*> areq checkBoxField "Disponible à la vente"
                (productAvailable <$> prod)
        <*> areq checkBoxField "Afficher en grand à la une de l'accueil"
                (productTop <$> prod)

-- | Supprime les produits et ses dépendances.
removeProduct :: ProductId -> YesodDB App ()
removeProduct prodId = do
    delete prodId

    pics <- selectList [PictureProduct ==. prodId] []
    forM_ pics $ \(Entity picId pic) -> do
        removePicture picId (pictureExtension pic)

-- -----------------------------------------------------------------------------

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
        $(widgetFile "pictures")

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
        $(widgetFile "pictures")
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

-- -----------------------------------------------------------------------------

redirectIfNotConnected :: Handler ()
redirectIfNotConnected = do
    v <- lookupSession sessionKey
    case v of
        Just _  -> return ()
        Nothing -> redirect AdminLoginR
