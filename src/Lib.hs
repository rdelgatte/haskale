module Lib
  ( runServer
  ) where

import Constants
import Control.Exception (try)
import Control.Monad.Except (ExceptT(..))
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Reader
import Data.Int (Int64)
import Data.List (find)
import Data.Text (Text)
import Database.Persist
import Database.Persist.Sqlite
import Model.Beer as B
import Network.Wai.Handler.Warp (defaultSettings, runSettings, setBeforeMainLoop, setPort)
import Persistence.DatabaseStuff
import Servant

type PingApi = "ping" :> Get '[ PlainText] Text

type BeerInput = ReqBody '[ JSON] Beer

type FindBeerById = "beers" :> Capture "id" Int64 :> Get '[ JSON] (Maybe Beer)

type AppContext = ReaderT SqlBackend IO

type CreateBeer = "beers" :> BeerInput :> PostNoContent '[ JSON] NoContent

type UpdateBeer = "beers" :> Capture "id" Int64 :> BeerInput :> Put '[ JSON] Int64

type ApplicationApi = PingApi :<|> "beers" :> Get '[ JSON] [Beer] :<|> FindBeerById :<|> CreateBeer :<|> UpdateBeer

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
      NoLoggingT $ runSettings settings $ serve proxyAPI (hoistAppServer sqlBackend)

hoistAppServer :: SqlBackend -> Server ApplicationApi
hoistAppServer sqlBackend = hoistServer proxyAPI contextToHandler server
  where
    contextToHandler :: AppContext a -> Handler a
    contextToHandler = Handler . ExceptT . try . (`runReaderT` sqlBackend)

proxyAPI :: Proxy ApplicationApi
proxyAPI = Proxy

pingHandler :: AppContext Text
pingHandler = return "Pong"

server :: ServerT ApplicationApi AppContext
server = pingHandler :<|> beersHandler :<|> beerByIdHandler :<|> createBeerHandler :<|> updateBeerHandler

beersHandler :: AppContext [Beer]
beersHandler = do
  beersFromDB :: [Entity BeerRow] <- selectList [] []
  return $ fromRow <$> beersFromDB

beerByIdHandler :: Int64 -> AppContext (Maybe Beer)
beerByIdHandler searched = pure (find (\beer -> B.id beer == Just searched) beers)

createBeerHandler :: Beer -> AppContext NoContent
createBeerHandler beer = do
  key <- insert $ toRow beer
  liftIO . putStrLn $ "Saved beer " <> show beer <> " with key = " <> show key
  return NoContent

updateBeerHandler :: Int64 -> Beer -> AppContext Int64
updateBeerHandler beerId beer = do
  replace (toSqlKey beerId) (toRow beer)
  liftIO . putStrLn $ "Updated beer " <> show beer
  return beerId
