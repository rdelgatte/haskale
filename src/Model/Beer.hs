module Model.Beer where

import           Data.Aeson
import           Data.Scientific
import           Data.Text
import           GHC.Generics
import           Model.BeerStyle

data Beer =
  Beer
    { id      :: Int
    , name    :: Text
    , style   :: BeerStyle
    , alcohol :: Maybe Scientific
    }
  deriving (Generic, Show, Eq)

instance ToJSON Beer
