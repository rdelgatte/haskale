{-# OPTIONS_GHC -fno-warn-orphans #-}

module Json.Swagger where

import Control.Lens ((&), (.~), (?~))
import Data.Swagger
import Servant.Swagger.UI (SwaggerSchemaUI)

-- |Servant type for the Swagger endpoints (UI and JSON)
type SwaggerUI = SwaggerSchemaUI "swagger-ui" "swagger.json"

-- |Add generic Swagger configuration like the title, description, etc.
enrichSwagger :: Swagger -> Swagger
enrichSwagger swagger =
  swagger & info . title .~ "Haskale" & info . description ?~
  "Let's have a look to build a HTTP API server in full Haskell" &
  info .
  contact ?~
  Contact
    { _contactName = Just "Haskale team"
    , _contactEmail = Nothing
    , _contactUrl = Just $ URL "https://github.com/rdelgatte/haskale"
    }
