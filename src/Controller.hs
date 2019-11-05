module Controller where

import Control.Monad.Reader
import Data.Text (Text)
import Database.Persist.Sqlite
import Servant

type ApplicationApi = Ping

type Ping = "ping" :> Get '[ PlainText] Text

type AppContext = ReaderT SqlBackend Handler

server :: ServerT ApplicationApi AppContext
server = pingHandler

pingHandler :: AppContext Text
pingHandler = return "Pong"
