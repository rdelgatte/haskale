module Model.BeerStyle where

import Data.Aeson
import Data.Swagger
import Database.Persist.TH
import GHC.Generics (Generic)
import Json.Custom

data BeerStyle
  = Altbier
  | AmberAle
  | BarleyWine
  | BerlinerWeisse
  | Garde
  | Bitter
  | BlondeAle
  | Bock
  | BrownAle
  | CaliforniaCommon
  | CreamAle
  | DortmunderExport
  | Doppelbock
  | Dunkel
  | Dunkelweizen
  | Eisbock
  | FlandersRedAle
  | Golden
  | Gose
  | Gueuze
  | Hefeweizen
  | Helles
  | IndiaPaleAle
  | Kolsch
  | Lambic
  | LightAle
  | Maibock
  | MaltLiquor
  | Mild
  | Oktoberfestbier
  | OldAle
  | OudBruin
  | PaleAle
  | Pils
  | Porter
  | RedAle
  | Roggenbier
  | Saison
  | ScotchAle
  | Stout
  | Schwarzbier
  | ViennaLager
  | Witbier
  | Weissbier
  | Weizenbock
  deriving (Generic, Show, Eq, Read)

derivePersistField "BeerStyle"

instance FromJSON BeerStyle where
  parseJSON = genericParseJSON upperSnakeOptions

instance ToJSON BeerStyle where
  toJSON = genericToJSON upperSnakeOptions

instance ToSchema BeerStyle where
  declareNamedSchema = genericDeclareNamedSchema upperSnakeSchema
