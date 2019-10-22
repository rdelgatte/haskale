module Constants where

import           Model.Beer
import           Model.BeerStyle

beers :: [Beer]
beers = [Beer {id = 1, name = "Brewdog IPA", style = IndiaPaleAle, alcohol = Just 5.4}]
