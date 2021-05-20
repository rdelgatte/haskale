module Controller where

import Control.Monad.Reader
import Data.Int (Int64)
import Data.Text (Text)
import Database.Persist
import Database.Persist.Sqlite
import Model.Beer
import Persistence.Database
import Servant

type ApplicationApi = Ping :<|> GetAllBeers :<|> FindBeerById :<|> CreateBeer :<|> UpdateBeer :<|> DeleteBeer

type Ping = "ping" :> Get '[ PlainText] Text

type BeerInput = ReqBody '[ JSON] Beer

type FindBeerById = "beers" :> Capture "id" Int64 :> Get '[ JSON] (Maybe Beer)

type AppContext = ReaderT SqlBackend Handler

type GetAllBeers = "beers" :> Get '[ JSON] [Beer]

type CreateBeer = "beers" :> BeerInput :> PostNoContent

type UpdateBeer = "beers" :> Capture "id" Int64 :> BeerInput :> Put '[ JSON] Int64

type DeleteBeer = "beers" :> Capture "id" Int64 :> Delete '[ JSON] NoContent

server :: ServerT ApplicationApi AppContext
server =
  pingHandler :<|> beersHandler :<|> beerByIdHandler :<|> createBeerHandler :<|> updateBeerHandler :<|>
  deleteBeerHandler

pingHandler :: AppContext Text
pingHandler = return "Pong"

beersHandler :: AppContext [Beer]
beersHandler = do
  beersFromDB :: [Entity BeerRow] <- selectList [] []
  return $ fromRow <$> beersFromDB

beerByIdHandler :: Int64 -> AppContext (Maybe Beer)
beerByIdHandler beerId = do
  beerRow <- getEntity beerKey
  return $ fmap fromRow beerRow
  where
    beerKey = toSqlKey beerId

createBeerHandler :: Beer -> AppContext NoContent
createBeerHandler beer = do
  key <- insert (toRow beer)
  liftIO (putStrLn ("Saved beer " ++ show beer ++ " with key = " ++ show key))
  return NoContent

updateBeerHandler :: Int64 -> Beer -> AppContext Int64
updateBeerHandler beerId beer = do
  maybeBeerRow <- get beerKey
  case maybeBeerRow of
    Nothing -> throwError err404
    Just _ -> do
      replace beerKey (toRow beer)
      liftIO . putStrLn $ "Updated beer " ++ show beer
      return beerId
  where
    beerKey = toSqlKey beerId

deleteBeerHandler :: Int64 -> AppContext NoContent
deleteBeerHandler beerId = do
  maybeBeerRow <- get beerKey
  case maybeBeerRow of
    Nothing -> throwError err404
    Just _ -> do
      delete beerKey
      liftIO . putStrLn $ "Deleted beer " <> show beerId
      return NoContent
  where
    beerKey :: Key BeerRow = toSqlKey beerId
