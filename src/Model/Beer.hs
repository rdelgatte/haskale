module Model.Beer where

import           Data.Aeson
import           Data.Int        (Int64)
import           Data.Scientific
import           Data.Text
import           GHC.Generics
import           Model.BeerStyle

data Beer =
  Beer
    { id      :: Maybe Int64
    , name    :: Text
    , style   :: BeerStyle
    , alcohol :: Maybe Scientific
    }
  deriving (Generic, Show, Eq)

instance ToJSON Beer

instance FromJSON Beer
