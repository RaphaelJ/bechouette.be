{-# LANGUAGE OverloadedStrings #-}
module Handler.Command (
      getCommandProductR, postCommandProductR
    , getCommandSpecialR, postCommandSpecialR
    , getCommandConfirmR
    ) where

import Import
import Control.Monad
import Data.Maybe
import Network.Mail.Mime (Address (..), simpleMail, renderSendMail)
import Text.Blaze.Renderer.Text (renderMarkup)

data Command = Command { 
      cLastName :: Text, cFirstName :: Text, cEmail :: Text, cTel :: Maybe Text
    , cNation :: Text, cShippingMethod :: ShippingMethod
    , cAddress :: Maybe Textarea, cDescription :: Textarea
    }

data ShippingMethod = OnSite | PostalDelivery deriving (Eq)

-- | Propose le formulaire de commande d'un produit particulier.
getCommandProductR :: ProductId -> Handler Html
getCommandProductR prodId = do
    (widget, enctype) <- generateFormPost productForm

    displayCommandProduct prodId widget enctype

postCommandProductR :: ProductId -> Handler Html
postCommandProductR prodId = do
    ((res, widget), enctype) <- runFormPost productForm

    case res of
        FormSuccess cmd -> do
            sendCommandMail (Just prodId) cmd
            redirect CommandConfirmR
        _               -> displayCommandProduct prodId widget enctype

displayCommandProduct :: ProductId -> Widget -> Enctype -> Handler Html
displayCommandProduct prodId widget enctype = do
    (prod, mPic) <- runDB $ (,) <$> get404 prodId
                                <*> (selectFirst [PictureProduct ==. prodId]
                                                 [Asc PictureId])

    when (not (productAvailable prod)) notFound

    defaultLayout $ do
        setTitle [shamlet|
                Commande d'un produit - #{productName prod} - Be Chouette
            |]
        $(widgetFile "command")
        $(widgetFile "commandproduct")

-- -----------------------------------------------------------------------------

-- | Propose le formulaire pour effectuer une demande personnalisée.
getCommandSpecialR :: Handler Html
getCommandSpecialR = do
    (widget, enctype) <- generateFormPost productForm

    displayCommandSpecial widget enctype

postCommandSpecialR :: Handler Html
postCommandSpecialR = do
    ((res, widget), enctype) <- runFormPost productForm

    case res of
        FormSuccess cmd -> do
            sendCommandMail Nothing cmd
            redirect CommandConfirmR
        _               -> displayCommandSpecial widget enctype

displayCommandSpecial :: Widget -> Enctype -> Handler Html
displayCommandSpecial widget enctype = do
    defaultLayout $ do
        setTitle [shamlet|Demande spéciale - Be Chouette|]
        $(widgetFile "command")
        $(widgetFile "commandspecial")

-- -----------------------------------------------------------------------------

-- | Affiche le message de confirmation de l'envoi de la demande.
getCommandConfirmR :: Handler Html
getCommandConfirmR = do
    defaultLayout $ do
        setTitle [shamlet|Confirmation de la demande - Be Chouette|]
        $(widgetFile "commandconfirm")

-- -----------------------------------------------------------------------------

productForm :: Form Command
productForm html = do
    (res, widget) <- flip renderDivs html $ Command
        <$> areq textField "Prénom" Nothing <*> areq textField "Nom" Nothing
        <*> areq emailField "Email" Nothing
        <*> aopt textField "Téléphone (facultatif)" Nothing
        <*> areq textField "Pays" Nothing
        <*> areq (selectFieldList shippingOpts) "Méthode de livraison souhaitée"
                 Nothing
        <*> aopt textareaField addressSetts Nothing
        <*> areq textareaField descriptionSetts Nothing

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
          ("Récupération sur place", OnSite)
        , ("Livraison postale", PostalDelivery)
        ]

    descriptionSetts = FieldSettings {
          fsLabel = "Description et remarques"
        , fsTooltip = Just "Spécifiez précisément les couleurs et tissus que \
                            vous souhaitez.\
                            N'hésitez pas à faire référence à d'autres produits\
                            en utilisant nos codes produits."
        , fsId = Nothing, fsName = Nothing, fsAttrs = []
        }

    addressSetts = FieldSettings {
          fsLabel = "Adresse de livraison"
        , fsTooltip = Just "Obligatoire si livraison postale.", fsId = Nothing
        , fsName = Nothing, fsAttrs = [("style", "width: 400px; height: 75px;")]
        }

sendCommandMail :: Maybe ProductId -> Command -> Handler ()
sendCommandMail mProdId cmd = do
    mProd <- case mProdId of
        Just prodId -> (Just . Entity prodId) <$> runDB (get404 prodId)
        Nothing     -> return Nothing

    rdr <- getUrlRenderParams

    email <- extraEmail <$> getExtra
    let to = Address Nothing email
        from = Address (Just $ cLastName cmd <> " " <> cFirstName cmd)
                       (cEmail cmd)
        subject = "Nouvelle demande sur Be Chouette"
        text = renderMarkup $ [hamlet|
            Une nouvelle demande sur Be Chouette !

            $maybe Entity prodId prod <- mProd
                Demande pour le produit "#{productName prod}" 
                $maybe ref <- productRef prod
                    Référence : #{ref}.
                @{ProductR prodId}
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
                Récupération sur place
            $else
                Livraison postale

            $maybe address <- cAddress cmd
                Adresse              : #{address}

            Description et remarques :

            #{cDescription cmd}
            |] rdr
        html = renderMarkup $ [hamlet|
            <html>
                <body>
                <h1>Une nouvelle demande sur Be Chouette !

                <p>
                    $maybe Entity prodId prod <- mProd
                        <a href=@{ProductR prodId}>
                            Demande pour le produit "#{productName prod}"
                            $maybe ref <- productRef prod
                                (ref : #{ref}).
                    $nothing
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
                                Récupération sur place
                            $else
                                Livraison postale
                    $maybe address <- cAddress cmd
                        <tr>
                            <td>Adresse
                            <td>#{address}

                    <tr>
                        <td>Description et remarques
                        <td>#{cDescription cmd}
            |] rdr

    liftIO $ simpleMail to from subject text html [] >>= renderSendMail
