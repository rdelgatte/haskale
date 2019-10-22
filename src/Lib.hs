module Lib
  ( runServer
  ) where

import Constants
import Data.Text (Text)
import Model.Beer
import Network.Wai.Handler.Warp (defaultSettings, runSettings, setBeforeMainLoop, setPort)
import Servant

type PingApi = "ping" :> Get '[ PlainText] Text

type ApplicationApi = PingApi :<|> "beers" :> Get '[ JSON] [Beer]

runServer :: IO ()
runServer = do
  let port = 3000
      settings = setPort port $ setBeforeMainLoop (putStrLn ("listening on port " ++ show port)) defaultSettings
  runSettings settings mkApp

mkApp :: Application
mkApp = serve (Proxy :: Proxy ApplicationApi) server

pingHandler :: Handler Text
pingHandler = return "Pong"

server :: Server ApplicationApi
server = pingHandler :<|> beersHandler

beersHandler :: Handler [Beer]
beersHandler = pure beers
