module Model.Beer where

import           Data.Aeson
import           Data.Int        (Int64)
import           Data.Scientific
import           Data.Swagger    (ToSchema)
import           Data.Text
import           GHC.Generics
import           Model.BeerStyle

data Beer =
  Beer
    { identifier :: Maybe Int64
    , name       :: Text
    , style      :: BeerStyle
    , alcohol    :: Maybe Scientific
    }
  deriving (Generic, Show, Eq)

instance ToJSON Beer

instance FromJSON Beer

instance ToSchema Beer
