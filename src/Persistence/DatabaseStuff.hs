{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Persistence.DatabaseStuff where

import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Scientific
import Data.Text
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import Model.Beer
import Model.BeerStyle

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
BeerRow
    name Text
    style BeerStyle
    alcohol Double Maybe
    deriving Show
|]

type DatabaseIO = ReaderT SqlBackend IO

saveBeer :: Beer -> DatabaseIO ()
saveBeer beer = do
  beerId <- insert $ toRow beer
  liftIO . putStrLn $ "Just saved beer " <> show beer <> " and its database ID is " <> show beerId

getAllBeers :: DatabaseIO [Beer]
getAllBeers = do
  beers :: [Entity BeerRow] <- selectList [] []
  liftIO . putStrLn $ "Beers in database are: " <> show beers
  return $ fromRow <$> beers

toRow :: Beer -> BeerRow
toRow Beer {..} = BeerRow {beerRowName = name, beerRowStyle = style, beerRowAlcohol = toRealFloat <$> alcohol}

fromRow :: Entity BeerRow -> Beer
fromRow Entity {entityKey, entityVal = BeerRow {..}} =
  Beer
    { id = Just $ fromSqlKey entityKey
    , name = beerRowName
    , style = beerRowStyle
    , alcohol = fromFloatDigits <$> beerRowAlcohol
    }
