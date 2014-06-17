{-# LANGUAGE DataKinds #-}
module Foundation where

import Prelude
import Data.Text (Text, pack, unpack)
import Yesod
import Yesod.Core.Types (Logger)
import Yesod.Static
import Yesod.Default.Config
import Yesod.Default.Util (addStaticContentExternal)
import Network.HTTP.Conduit (Manager)
import qualified Settings
import Settings.Development (development)
import qualified Database.Persist
import Settings.StaticFiles
import Database.Persist.Sql
import Model
import Settings (PersistConf, widgetFile, Extra (..))
import System.FilePath ((</>))
import Text.Jasmine (minifym)
import Text.Hamlet (hamletFile)
import Text.Printf
import Yesod.Form.Bootstrap3

-- | The site argument for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data App = App
    { settings :: AppConfig DefaultEnv Extra
    , getStatic :: Static -- ^ Settings for static file serving.
    , connPool :: Database.Persist.PersistConfigPool PersistConf -- ^ Database connection pool.
    , httpManager :: Manager
    , persistConfig :: PersistConf
    , appLogger :: Logger
    }

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://www.yesodweb.com/book/handler
--
-- This function does three things:
--
-- * Creates the route datatype AppRoute. Every valid URL in your
--   application can be represented as a value of this type.
-- * Creates the associated type:
--       type instance Route App = AppRoute
-- * Creates the value resourcesApp which contains information on the
--   resources declared below. This is used in Handler.hs by the call to
--   mkYesodDispatch
--
-- What this function does *not* do is create a YesodSite instance for
-- App. Creating that instance requires all of the handler functions
-- for our application to be in scope. However, the handler functions
-- usually require access to the AppRoute datatype. Therefore, we
-- split these actions into two functions and place them in separate files.
mkYesodData "App" $(parseRoutesFile "config/routes")

type Form x = Html -> MForm (HandlerT App IO) (FormResult x, Widget)

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App where
    approot = ApprootMaster $ appRoot . settings

    -- Store session data on the client in encrypted cookies,
    -- default session idle timeout is 120 minutes
    makeSessionBackend _ = fmap Just $ defaultClientSessionBackend
        (120 * 60) -- 120 minutes
        "config/client_session_key.aes"

    defaultLayout widget = do
        mmsg <- getMessage

        currentRoute <- getCurrentRoute

        -- We break up the default layout into two components:
        -- default-layout is the contents of the body tag, and
        -- default-layout-wrapper is the entire page. Since the final
        -- value passed to hamletToRepHtml cannot be a widget, this allows
        -- you to use normal widget features in default-layout.

        pc <- widgetToPageContent $ do
            addStylesheetRemote "//fonts.googleapis.com/css?family=Lato:100italic,100,300italic,300,400italic,400,700italic,700,900italic,900"
            addScriptRemote "//ajax.googleapis.com/ajax/libs/jquery/1.10.2/jquery.min.js"
            addStylesheetRemote "//netdna.bootstrapcdn.com/bootstrap/3.0.3/css/bootstrap.min.css"
            addScriptRemote "//netdna.bootstrapcdn.com/bootstrap/3.0.3/js/bootstrap.min.js"

            $(widgetFile "default-layout")
        giveUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

    -- This is done to provide an optimization for serving static files from
    -- a separate domain. Please see the staticRoot setting in Settings.hs
    urlRenderOverride y (StaticR s) =
        Just $ uncurry (joinPath y (Settings.staticRoot $ settings y)) $ renderRoute s
    urlRenderOverride _ _ = Nothing

    -- The page to be redirected to when authentication is required.
    authRoute _ = Nothing

    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
    addStaticContent =
        addStaticContentExternal minifym genFileName Settings.staticDir (StaticR . flip StaticRoute [])
      where
        -- Generate a unique filename based on the content itself
        genFileName lbs
            | development = "autogen-" ++ base64md5 lbs
            | otherwise   = base64md5 lbs

    maximumContentLength _ (Just (AdminPicturesR _ )) = Nothing
    maximumContentLength _ _                          = Just $ 2 * 1024 * 1024

    -- Place Javascript at bottom of the body tag so the rest of the page loads first
    jsLoader _ = BottomOfBody

    -- What messages should be logged. The following includes all messages when
    -- in development, and warnings and errors in production.
    shouldLog _ _source level =
        development || level == LevelWarn || level == LevelError

    makeLogger = return . appLogger

-- How to run database actions.
instance YesodPersist App where
    type YesodPersistBackend App = SqlPersistT
    runDB f = do
        master <- getYesod
        Database.Persist.runPool
            (persistConfig master)
            f
            (connPool master)

-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

-- | Get the 'Extra' value, used to hold data from the settings.yml file.
getExtra :: Handler Extra
getExtra = fmap (appExtra . settings) getYesod

data PicType = PicOriginal
             | PicSmall -- 100x100
             | PicLarge -- 350x400
             | PicWide -- 1170x300
             | PicCatalogue -- 300x95

routePicture :: PictureId -> PicType -> Text -> Route App
routePicture picId picType picExt =
    StaticR $ StaticRoute ["pictures", pack $ picName picId picType picExt] []

picPath :: PictureId -> PicType -> Text -> FilePath
picPath picId picType picExt =
    Settings.pictureDir </> picName picId picType picExt

picName :: PictureId -> PicType -> Text -> String
picName ~(Key (PersistInt64 picId)) picType picExt =
    printf "%s%s.%s" (show picId) picSuffix (unpack picExt)
  where
    picSuffix = case picType of
        PicOriginal  -> "" :: String
        PicSmall     -> "_small"
        PicLarge     -> "_large"
        PicWide      -> "_wide"
        PicCatalogue -> "_catalogue"

-- Note: previous versions of the scaffolding included a deliver function to
-- send emails. Unfortunately, there are too many different options for us to
-- give a reasonable default. Instead, the information is available on the
-- wiki:
--
-- https://github.com/yesodweb/yesod/wiki/Sending-email

bootstrapSubmit' :: MonadHandler m => BootstrapSubmit Text -> AForm m ()
bootstrapSubmit' setts = bootstrapSubmit setts { bsClasses = "btn-primary"}

bfs' :: Text -> FieldSettings site
bfs' = bfs
