{-# LANGUAGE OverloadedStrings #-}
module Handler.Command (
      getCommandCatalogR, postCommandCatalogR
    , getCommandBirthListR, postCommandBirthListR
    , getCommandSpecialR, postCommandSpecialR
    , getCommandConfirmR
    ) where

import Import
import Control.Applicative
import Control.Monad
import Data.Maybe
import Network.Mail.Mime (Address (..), simpleMail, renderSendMail)
import Text.Blaze.Renderer.Text (renderMarkup)
import Yesod.Form.Bootstrap3

import Handler.Catalog (productPic)

data Command = Command { 
      cLastName :: Text, cFirstName :: Text, cEmail :: Text, cTel :: Maybe Text
    , cNation :: Text, cShippingMethod :: ShippingMethod
    , cAddress :: Maybe Textarea, cDescription :: Maybe Textarea
    }

data ShippingMethod = OnSite | PostalDelivery deriving (Eq)

data CommandType = CatalogCommand SubCategoryId SubCategoryProductId
                 | BirthListCommand BirthListId BirthListProductId
                 | SpecialCommand

-- | Propose le formulaire de commande d'un produit particulier.
getCommandCatalogR :: SubCategoryId -> SubCategoryProductId -> Handler Html
getCommandCatalogR subCatId subCatProdId = do
    (widget, enctype) <- generateFormPost (productForm True)

    displayCatalogCommand subCatId subCatProdId widget enctype

postCommandCatalogR :: SubCategoryId -> SubCategoryProductId -> Handler Html
postCommandCatalogR subCatId subCatProdId = do
    ((res, widget), enctype) <- runFormPost (productForm True)

    case res of
        FormSuccess cmd -> do
            sendCommandMail (CatalogCommand subCatId subCatProdId) cmd
            redirect CommandConfirmR
        _               ->
            displayCatalogCommand subCatId subCatProdId widget enctype

displayCatalogCommand :: SubCategoryId -> SubCategoryProductId -> Widget
                      -> Enctype -> Handler Html
displayCatalogCommand subCatId subCatProdId widget enctype = do
    (prod, mPic) <- runDB $ do
        _          <- get404 subCatId
        subCatProd <- get404 subCatProdId
        let prodId = subCategoryProductProduct subCatProd
        prod       <- getJust prodId
        mPic       <- productPic prodId
        return (prod, mPic)

    when (not (productAvailable prod)) notFound

    defaultLayout $ do
        setTitle [shamlet|
                Commande d'un produit - #{productName prod} - Be Chouette
            |]
        $(widgetFile "command")
        $(widgetFile "command-catalog")

-- -----------------------------------------------------------------------------

-- | Propose le formulaire de réservation d'un produit d'une liste de naissance.
getCommandBirthListR :: BirthListId -> BirthListProductId -> Handler Html
getCommandBirthListR blId blProdId = do
    (widget, enctype) <- generateFormPost (productForm False)

    displayBirthListCommand blId blProdId widget enctype

postCommandBirthListR :: BirthListId -> BirthListProductId -> Handler Html
postCommandBirthListR blId blProdId = do
    ((res, widget), enctype) <- runFormPost (productForm False)

    case res of
        FormSuccess cmd -> do
            sendCommandMail (BirthListCommand blId blProdId) cmd
            redirect CommandConfirmR
        _               ->
            displayBirthListCommand blId blProdId widget enctype

displayBirthListCommand :: BirthListId -> BirthListProductId -> Widget
                        -> Enctype -> Handler Html
displayBirthListCommand blId blProdId widget enctype = do
    (bl, prod, mPic) <- runDB $ do
        bl     <- get404 blId
        blProd <- get404 blProdId
        let prodId = birthListProductProduct blProd
        prod   <- getJust prodId
        mPic   <- productPic prodId
        return (bl, prod, mPic)

    defaultLayout $ do
        setTitle [shamlet|
                Réserver un produit - #{productName prod} - Be Chouette
            |]
        $(widgetFile "command")
        $(widgetFile "command-birthlist")

-- -----------------------------------------------------------------------------

-- | Propose le formulaire pour effectuer une demande personnalisée.
getCommandSpecialR :: Handler Html
getCommandSpecialR = do
    (widget, enctype) <- generateFormPost (productForm True)

    displayCommandSpecial widget enctype

postCommandSpecialR :: Handler Html
postCommandSpecialR = do
    ((res, widget), enctype) <- runFormPost (productForm True)

    case res of
        FormSuccess cmd -> do
            sendCommandMail SpecialCommand cmd
            redirect CommandConfirmR
        _               -> displayCommandSpecial widget enctype

displayCommandSpecial :: Widget -> Enctype -> Handler Html
displayCommandSpecial widget enctype = do
    defaultLayout $ do
        setTitle [shamlet|Demande spéciale - Be Chouette|]
        $(widgetFile "command")
        $(widgetFile "command-special")

-- -----------------------------------------------------------------------------

-- | Affiche le message de confirmation de l'envoi de la demande.
getCommandConfirmR :: Handler Html
getCommandConfirmR = do
    defaultLayout $ do
        setTitle [shamlet|Confirmation de la demande - Be Chouette|]
        $(widgetFile "command-confirm")

-- -----------------------------------------------------------------------------

productForm :: Bool -> Form Command
productForm describe html = do
    (res, widget) <- flip (renderBootstrap3 BootstrapBasicForm) html $ Command
        <$> areq textField  (bfs' "Prénom") Nothing
        <*> areq textField  (bfs' "Nom") Nothing
        <*> areq emailField (bfs' "Email") Nothing
        <*> aopt textField  (bfs' "Téléphone (facultatif)") Nothing
        <*> areq textField  (bfs' "Pays") Nothing
        <*> areq (selectFieldList shippingOpts)
                 (bfs' "Méthode de retrait souhaitée")
                 Nothing
        <*> aopt textareaField addressSetts Nothing
        <*> aopt textareaField descriptionSetts Nothing
        <*  bootstrapSubmit' "Soumettre ma demande"

    return $ case res of
        FormSuccess cmd | cShippingMethod cmd == PostalDelivery
                          && isNothing (cAddress cmd) ->
            let msg = "L'adresse de livraison doit être renseignée." :: Text
                widget' = [whamlet|
                    <p .errors>#{msg}
                    ^{widget}
                    |]
            in (FormFailure [msg], widget')
        _ -> (res, widget)
  where
    shippingOpts :: [(Text, ShippingMethod)]
    shippingOpts = [
          ("Retrait sur place", OnSite)
        , ("Envoi postale (+ frais de port)", PostalDelivery)
        ]

    descriptionSetts =
        let baseSetts | describe  =
                (bfs' "Description et remarques") {
                      fsTooltip = Just "Spécifiez précisément les couleurs et tissus que \
                                        vous souhaitez.\
                                        N'hésitez pas à faire référence à d'autres produits\
                                        en utilisant nos codes produits."
                    }
                      | otherwise = bfs' "Remarques"
        in baseSetts { fsAttrs = [("class", "form-control description")] }

    addressSetts = (bfs' "Adresse de livraison") {
          fsTooltip = Just "Obligatoire si livraison postale."
        }

sendCommandMail :: CommandType -> Command -> Handler ()
sendCommandMail cmdType cmd = do
    mProd <- case cmdType of
                CatalogCommand subCatId subCatProdId ->
                    runDB $ do
                        _          <- get404 subCatId
                        subCatProd <- get404 subCatProdId
                        get $ subCategoryProductProduct subCatProd
                BirthListCommand blId blProdId       ->
                    runDB $ do
                        _      <- get404 blId
                        blProd <- get404 blProdId
                        get $ birthListProductProduct blProd
                SpecialCommand                       ->
                    return Nothing

    rdr <- getUrlRenderParams

    email <- extraEmail <$> getExtra
    let to = Address Nothing email
        from = Address (Just $ cLastName cmd <> " " <> cFirstName cmd)
                       (cEmail cmd)
        subject = "Nouvelle demande sur Be Chouette"
        text = renderMarkup $ [hamlet|
            Une nouvelle demande sur Be Chouette !

            $maybe prod <- mProd
                Demande pour le produit "#{productName prod}" 
                $maybe ref <- productRef prod
                    Référence : #{ref}.

                $case cmdType
                    $of CatalogCommand subCatId subCatProdId
                        Lien : @{SubCategoryProductR subCatId subCatProdId}
                    $of BirthListCommand blId blProdId
                        Lien : @{BirthListProductR blId blProdId}
                    $of _
            $nothing
                Demande personnalisée.

            Prénom               : #{cLastName cmd}
            Nom                  : #{cFirstName cmd}
            Email                : #{cEmail cmd}
            $maybe tel <- cTel cmd
                Téléphone            : #{tel}
            Pays                 : #{cNation cmd}

            Méthode de livraison :
            $if cShippingMethod cmd == OnSite
                Retrair sur place
            $else
                Envoi postal

            $maybe address <- cAddress cmd
                Adresse              : #{address}

            Description et remarques :

            $maybe desc <- cDescription cmd
                #{desc}
            $nothing
                Aucune.
            |] rdr
        html = renderMarkup $ [hamlet|
            <html>
                <body>
                <h1>Une nouvelle demande sur Be Chouette !

                $maybe prod <- mProd
                    <p>
                        Demande pour le produit "#{productName prod}"
                        $maybe ref <- productRef prod
                            (référence : #{ref})

                    $case cmdType
                        $of CatalogCommand subCatId subCatProdId
                            <p>
                                Lien :
                                <a href=@{SubCategoryProductR subCatId subCatProdId}>
                                    @{SubCategoryProductR subCatId subCatProdId}
                        $of BirthListCommand blId blProdId
                            <p>
                                Lien :
                                <a href=@{BirthListProductR blId blProdId}>
                                    @{BirthListProductR blId blProdId}
                        $of _
                $nothing
                    <p>
                        Demande personnalisée.

                <table>
                    <tr>
                        <td>Prénom
                        <td>#{cLastName cmd}
                    <tr>
                        <td>Nom
                        <td>#{cFirstName cmd}
                    <tr>
                        <td>Email
                        <td>#{cEmail cmd}
                    $maybe tel <- cTel cmd
                        <tr>
                            <td>Téléphone
                            <td>#{tel}
                    <tr>
                        <td>Pays
                        <td>#{cNation cmd}
                    <tr>
                        <td>Méthode de livraison
                        <td>
                            $if cShippingMethod cmd == OnSite
                                Retrait sur place
                            $else
                                Envoi postal
                    $maybe address <- cAddress cmd
                        <tr>
                            <td>Adresse
                            <td>#{address}

                    <tr>
                        <td>Description et remarques
                        <td>
                            $maybe desc <- cDescription cmd
                                #{desc}
                            $nothing
                                Aucune.
            |] rdr

    liftIO $ simpleMail to from subject text html [] >>= renderSendMail
