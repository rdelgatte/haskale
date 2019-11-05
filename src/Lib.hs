module Lib
  ( runServer
  ) where

import Control.Exception (try)
import Control.Monad.Except (ExceptT(..))
import Control.Monad.Logger
import Control.Monad.Reader
import Data.Text (Text)
import Database.Persist.Sqlite as P
import Network.Wai.Handler.Warp (defaultSettings, runSettings, setBeforeMainLoop, setPort)
import Persistence.Database
import Servant

type Ping = "ping" :> Get '[ PlainText] Text

type AppContext = ReaderT SqlBackend IO

runServer :: IO ()
runServer = do
  let port = 3000
      settings = setPort port $ setBeforeMainLoop (putStrLn ("listening on port " ++ show port)) defaultSettings
  runNoLoggingT $
    withSqliteConn ":memory:" $ \sqlBackend
      -- Do the database migration once
     -> do
      runReaderT (runMigration migrateAll) sqlBackend
      -- THEN run the server with the database in the up-to-date schema
      NoLoggingT $ runSettings settings (application sqlBackend)

application :: SqlBackend -> Application
application sqlBackend = serve proxyAPI (hoistAppServer sqlBackend)

hoistAppServer :: SqlBackend -> Server Ping
hoistAppServer sqlBackend = hoistServer proxyAPI contextToHandler server
  where
    contextToHandler :: AppContext a -> Handler a
    contextToHandler = Handler . ExceptT . try . (`runReaderT` sqlBackend)

proxyAPI :: Proxy Ping
proxyAPI = Proxy

pingHandler :: AppContext Text
pingHandler = return "Pong"

server :: ServerT Ping AppContext
server = pingHandler
