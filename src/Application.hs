module Application
  ( runServer
  ) where

import           Control.Monad.Logger
import           Control.Monad.Reader
import           Database.Persist.Sqlite
import           Network.Wai.Handler.Warp (defaultSettings, runSettings,
                                           setBeforeMainLoop, setPort)
import           Persistence.Database
import           Servant

import           Controller

runServer :: IO ()
runServer = do
  let port = 3000
      settings = setPort port $ setBeforeMainLoop (putStrLn ("listening on port " ++ show port)) defaultSettings
  runNoLoggingT $ withSqliteConn ":memory:" $ \sqlBackend -> do
    runReaderT (runMigration migrateAll) sqlBackend -- Do the database migration once
    NoLoggingT $ runSettings settings (application sqlBackend) -- THEN run the server with the database in the up-to-date schema

application :: SqlBackend -> Application
application sqlBackend = serve proxyAPI (hoistAppServer sqlBackend)

hoistAppServer :: SqlBackend -> Server ApplicationApi
hoistAppServer sqlBackend = hoistServer proxyAPI contextToHandler server
  where
    contextToHandler :: AppContext a -> Handler a
    contextToHandler = (`runReaderT` sqlBackend)

proxyAPI :: Proxy ApplicationApi
proxyAPI = Proxy
