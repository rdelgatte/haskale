module Lib
  ( runServer
  ) where

import Constants
import Data.List (find)
import Data.Text (Text)
import Model.Beer as B
import Network.Wai.Handler.Warp (defaultSettings, runSettings, setBeforeMainLoop, setPort)
import Servant

type PingApi = "ping" :> Get '[ PlainText] Text

type BeerInput = ReqBody '[ JSON] Beer

type FindBeerById = "beers" :> Capture "id" Int :> Get '[ JSON] (Maybe Beer)

type CreateBeer = "beers" :> BeerInput :> PostNoContent '[ JSON] NoContent

type ApplicationApi = PingApi :<|> "beers" :> Get '[ JSON] [Beer] :<|> FindBeerById :<|> CreateBeer

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
server = pingHandler :<|> beersHandler :<|> beerByIdHandler :<|> createBeerHandler

beersHandler :: Handler [Beer]
beersHandler = pure beers

beerByIdHandler :: Int -> Handler (Maybe Beer)
beerByIdHandler searched = pure (find (\beer -> B.id beer == searched) beers)

createBeerHandler :: Beer -> Handler NoContent
createBeerHandler beer = pure NoContent
