{-# LANGUAGE OverloadedStrings #-}
module Handler.Admin (
      getAdminR, postAdminR, getAdminLoginR, postAdminLoginR, getAdminRemoveCatR
    , getAdminNewProdR, postAdminNewProdR, getAdminEditProdR, postAdminEditProdR
    , getAdminRemoveProdR, getAdminPicturesR, postAdminPicturesR
    ) where

import Import
import qualified Control.Exception as E
import Control.Monad
import System.Directory
import qualified Vision.Image as I
import qualified Vision.Primitive as I

import Handler.Home (listProducts)

sessionKey :: Text
sessionKey = "CONNECTED"

getAdminR :: Handler RepHtml
getAdminR = do
    redirectIfNotConnected

    (widget, enctype) <- generateFormPost categoryForm

    cats <- runDB listProducts

    let err = Nothing :: Maybe Text
    defaultLayout $ do
        setTitle "Administration - Be Chouette"
        $(widgetFile "admin")

-- | Ajout d'une nouvelle catégorie.
postAdminR :: Handler RepHtml
postAdminR = do
    redirectIfNotConnected

    ((result, widget), enctype) <- runFormPost categoryForm

    err <- case result of
        FormSuccess name -> do
            m <- runDB $ insertUnique (Category name)
            case m of
                Just _  -> return (Nothing :: Maybe Text)
                Nothing -> return $! Just "Nom de catégorie existant."
        _ -> 
            return Nothing

    cats <- runDB listProducts

    defaultLayout $ do
        setTitle "Administration - Be Chouette"
        $(widgetFile "admin")

getAdminRemoveCatR :: CategoryId -> Handler ()
getAdminRemoveCatR catId = do
    redirectIfNotConnected

    runDB $ do
        _ <- delete catId
        prods <- selectList [ProductCategory ==. catId] []
        forM_ prods (removeProduct . entityKey)

    redirect AdminR

categoryForm :: Form Text
categoryForm = renderDivs $ areq textField "Nom de la catégorie : " Nothing

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
loginForm = renderDivs $ areq passwordField "Mot de passe : " Nothing

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
    prod <- runDB $ get404 prodId
    let catId = productCategory prod

    (widget, enctype) <- generateFormPost (prodForm catId (Just prod))

    defaultLayout $ do
        setTitle "Modifier un  produit - Be Chouette"
        $(widgetFile "editprod")

postAdminEditProdR :: ProductId -> Handler RepHtml
postAdminEditProdR prodId = do
    prod <- runDB $ get404 prodId
    let catId = productCategory prod

    ((result, widget), enctype) <- runFormPost (prodForm catId (Just prod))

    case result of
        FormSuccess newProd -> do
            _ <- runDB $ replace prodId newProd
            redirect AdminR
        _ -> do
            defaultLayout $ do
                setTitle "Modifier un  produit - Be Chouette"
                $(widgetFile "editprod")

getAdminRemoveProdR :: ProductId -> Handler ()
getAdminRemoveProdR prodId = do
    redirectIfNotConnected

    runDB $ removeProduct prodId
    redirect AdminR

prodForm :: CategoryId -> Maybe Product -> Form Product
prodForm catId prod = renderTable $
    Product catId <$> areq textField "Nom du produit : " (productName <$> prod)
                  <*> areq textField "Description rapide : " 
                           (productShortDesc <$> prod)
                  <*> (unTextarea <$>
                        areq textareaField "Description complète : "
                             (Textarea <$> productDesc <$> prod))
                  <*> (unTextarea <$>
                        areq textareaField "Détails (tailles, couleurs etc) : "
                             (Textarea <$> productDetails <$> prod))
                  <*> areq doubleField 
                           "Prix ('.' pour séparer les décimales) : " 
                           (productPrice <$> prod)
                  <*> areq checkBoxField "Afficher à la une : " 
                           (productTopProduct <$> prod)

-- | Supprime les produits et ses dépendances.
removeProduct :: ProductId -> YesodDB sub App ()
removeProduct prodId = do
    delete prodId

    pics <- selectList [PictureProduct ==. prodId] []
    forM_ pics $ \(Entity picId _) -> do
        delete picId

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
        FormSuccess info -> processImage info
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
        picId <- runDB $ insert (Picture prodId)

        let path = pathPicture picId PicOriginal
        fileMove info path

        mImg <- liftIO $ C.try $ I.load path
        case mImg of
            Just img -> do
                resizeImage img PicSmall
                resizeImage img PicLarge
                resizeImage img PicWide
                resizeImage img PicCatalogue
                return Nothing
            Nothing  -> do
                removeFile path
                runDB $ delete picId
                return $ Just ("Image invalide." :: Text)

    resizeImage img picType = do
        let I.Size w h = I.getSize img
            I.Size w' h' = picSize picType
            ratio = min (w % w') (h % h')
            (tmpW, tmpH) = (truncate $ w / ratio, truncate $ h / ratio)
            tmp = I.resize I.LinearInterpol img (I.Size tmpW tmpH)
            img' = I.crop tmp 
        I.resize I.LinearInterpol $ I.crop img 

    picSize PicSmall     = I.Size 100 100
    picSize PicLarge     = I.Size 300 400
    picSize PicWide      = I.Size 950 300
    picSize PicCatalogue = I.Size 300 95

postAdminPicturesR :: ProductId -> Handler RepHtml
postAdminPicturesR = getAdminPicturesR

pictureForm :: Form FileInfo
pictureForm = renderDivs $ fileAFormReq "Fichier de l'image : "

-- -----------------------------------------------------------------------------

redirectIfNotConnected :: Handler ()
redirectIfNotConnected = do
    v <- lookupSession sessionKey
    case v of
        Just _  -> return ()
        Nothing -> redirect AdminLoginR