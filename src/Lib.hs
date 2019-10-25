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
import Data.Swagger (Swagger)
import Data.Text (Text)
import Database.Persist
import Database.Persist.Sqlite as P
import Model.Beer as B
import Network.Wai.Handler.Warp (defaultSettings, runSettings, setBeforeMainLoop, setPort)
import Persistence.DatabaseStuff
import Servant
import Servant.Swagger
import Servant.Swagger.UI

-- |Servant type for the Swagger endpoints (UI and JSON)
type SwaggerUI = SwaggerSchemaUI "swagger-ui" "swagger.json"

type PingApi = "ping" :> Get '[ PlainText] Text

type BeerInput = ReqBody '[ JSON] Beer

type FindBeerById = "beers" :> Capture "id" Int64 :> Get '[ JSON] (Maybe Beer)

type AppContext = ReaderT SqlBackend IO

type GetAllBeers = "beers" :> Get '[ JSON] [Beer]

type CreateBeer = "beers" :> BeerInput :> PostNoContent '[ JSON] NoContent

type UpdateBeer = "beers" :> Capture "id" Int64 :> BeerInput :> Put '[ JSON] Int64

type DeleteBeer = "beers" :> Capture "id" Int64 :> Delete '[ JSON] NoContent

type ApplicationApi = PingApi :<|> GetAllBeers :<|> FindBeerById :<|> CreateBeer :<|> UpdateBeer :<|> DeleteBeer

type ApiWithSwagger = SwaggerUI :<|> ApplicationApi

swaggerDoc :: Swagger
swaggerDoc = toSwagger (Proxy :: Proxy ApplicationApi)

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
      NoLoggingT $ runSettings settings $ serve proxyAPIWithSwagger (hoistAppServer sqlBackend)

swaggerServer :: Server SwaggerUI
swaggerServer = swaggerSchemaUIServer swaggerDoc

hoistAppServer :: SqlBackend -> Server ApiWithSwagger
hoistAppServer sqlBackend = swaggerServer :<|> hoistServer proxyAPI contextToHandler server
  where
    contextToHandler :: AppContext a -> Handler a
    contextToHandler = Handler . ExceptT . try . (`runReaderT` sqlBackend)

proxyAPI :: Proxy ApplicationApi
proxyAPI = Proxy

proxyAPIWithSwagger :: Proxy ApiWithSwagger
proxyAPIWithSwagger = Proxy

pingHandler :: AppContext Text
pingHandler = return "Pong"

server :: ServerT ApplicationApi AppContext
server =
  pingHandler :<|> beersHandler :<|> beerByIdHandler :<|> createBeerHandler :<|> updateBeerHandler :<|>
  deleteBeerHandler

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

deleteBeerHandler :: Int64 -> AppContext NoContent
deleteBeerHandler beerId = do
  P.delete (toSqlKey beerId :: Key BeerRow)
  liftIO . putStrLn $ "Deleted beer " <> show beerId
  return NoContent
