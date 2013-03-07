{-# LANGUAGE OverloadedStrings #-}
module Handler.Admin (
      getAdminR, postAdminR, getAdminLoginR, postAdminLoginR, getAdminRemoveCatR
    , getAdminNewProdR, postAdminNewProdR, getAdminRemoveProdR
    , getAdminPicturesR, postAdminPicturesR
    ) where

import Import
import Control.Monad

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

    (widget, enctype) <- generateFormPost (newProdForm catId)

    defaultLayout $ do
        setTitle "Ajouter un nouveau produit - Be Chouette"
        $(widgetFile "newprod")

postAdminNewProdR :: CategoryId -> Handler RepHtml
postAdminNewProdR catId = do
    redirectIfNotConnected

    ((result, widget), enctype) <- runFormPost (newProdForm catId)

    case result of
        FormSuccess prod -> do
            _ <- runDB $ get404 catId >> insert prod
            redirect AdminR
        _ -> do
            cat <- runDB $ get404 catId
            defaultLayout $ do
                setTitle "Ajouter un nouveau produit - Be Chouette"
                $(widgetFile "newprod")

getAdminRemoveProdR :: ProductId -> Handler ()
getAdminRemoveProdR prodId = do
    redirectIfNotConnected

    runDB $ removeProduct prodId
    redirect AdminR

newProdForm :: CategoryId -> Form Product
newProdForm catId = renderTable $
    Product catId <$> areq textField     "Nom du produit : " Nothing
                  <*> areq textField     "Description rapide : " Nothing
                  <*> (unTextarea <$>
                        areq textareaField "Description complète : " Nothing)
                  <*> (unTextarea <$>
                        areq textareaField "Détails (tailles, couleurs etc) : "
                             Nothing)
                  <*> areq doubleField   "Prix : " Nothing
                  <*> areq checkBoxField "Afficher à la une : " Nothing

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

    (prod, pics) <- runDB $ do
        prod <- get404 prodId
        pics <- selectList [PictureProduct ==. prodId] [Asc PictureId]
        return (prod, pics)

    (widget, enctype) <- generateFormPost pictureForm

    let err = Nothing :: Maybe Text
    defaultLayout $ do
        setTitle "Gestion des photos - Be Chouette"
        $(widgetFile "pictures")

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