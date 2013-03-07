module Model where

import Prelude
import Yesod
import Data.Text (Text)
import Database.Persist.Quasi

share [mkPersist sqlOnlySettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")
