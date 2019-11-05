module Constants where

import           Model.Beer
import           Model.BeerStyle

beers :: [Beer]
beers =
  [ Beer {identifier = Just 42, name = "Brewdog IPA", style = IndiaPaleAle, alcohol = Just 5.4}
  , Beer {identifier = Just 2, name = "Anosteke", style = Saison, alcohol = Just 5}
  , Beer {identifier = Just 21, name = "Trappe Quadrupel", style = AmberAle, alcohol = Just 10}
  , Beer {identifier = Just 14, name = "Rince Cochon", style = BlondeAle, alcohol = Just 8.5}
  , Beer {identifier = Just 8, name = "Delirium Tremens", style = BlondeAle, alcohol = Just 8.5}
  ]
