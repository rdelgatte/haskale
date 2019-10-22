module Lib
  ( runServer
  ) where

import Network.Wai.Handler.Warp (defaultSettings, runSettings, setBeforeMainLoop, setPort)

import Data.Text (Text)
import Servant

type Ping = "ping" :> Get '[ PlainText] Text

runServer :: IO ()
runServer = do
  let port = 3000
      settings = setPort port $ setBeforeMainLoop (putStrLn ("listening on port " ++ show port)) defaultSettings
  runSettings settings mkApp

mkApp :: Application
mkApp = serve (Proxy :: Proxy Ping) server

pingHandler :: Handler Text
pingHandler = return "Pong"

server :: Server Ping
server = pingHandler
