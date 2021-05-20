module Application
  ( runServer
  ) where

import Control.Lens
import Control.Monad.Logger
import Control.Monad.Reader
import Data.Swagger (Swagger, description, info, title)
import Database.Persist.Sqlite
import Network.Wai.Handler.Warp (defaultSettings, runSettings, setBeforeMainLoop, setPort)
import Persistence.Database
import Servant
import Servant.Swagger (toSwagger)
import Servant.Swagger.UI

import Controller

-- |Servant type for the Swagger endpoints (UI and JSON)
type SwaggerUI = SwaggerSchemaUI "swagger-ui" "swagger.json"

type ApiWithSwagger = ApplicationApi :<|> SwaggerUI

swaggerDoc :: Swagger
swaggerDoc = enrichSwagger $ toSwagger (Proxy :: Proxy ApplicationApi)

runServer :: IO ()
runServer = do
  let port = 3000
      settings = setPort port $ setBeforeMainLoop (putStrLn ("listening on port " ++ show port)) defaultSettings
  runNoLoggingT $ withSqliteConn ":memory:" $ \sqlBackend -> do
    runReaderT (runMigration migrateAll) sqlBackend -- Do the database migration once
    NoLoggingT $ runSettings settings (application sqlBackend) -- THEN run the server with the database in the up-to-date schema

application :: SqlBackend -> Application
application sqlBackend = serve proxyAPIWithSwagger (hoistAppServer sqlBackend :<|> swaggerServer)

swaggerServer :: Server SwaggerUI
swaggerServer = swaggerSchemaUIServer swaggerDoc

hoistAppServer :: SqlBackend -> Server ApplicationApi
hoistAppServer sqlBackend = hoistServer proxyAPI contextToHandler server
  where
    contextToHandler :: AppContext a -> Handler a
    contextToHandler = (`runReaderT` sqlBackend)

proxyAPI :: Proxy ApplicationApi
proxyAPI = Proxy

proxyAPIWithSwagger :: Proxy ApiWithSwagger
proxyAPIWithSwagger = Proxy

enrichSwagger :: Swagger -> Swagger
enrichSwagger swagger =
  swagger & info . title .~ "Hask Ale" & info . description ?~
  "Hask Ale is a workshop support to code Haskell REST APIs using Servant"
