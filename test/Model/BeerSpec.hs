module Model.BeerSpec where

import Data.Aeson (decode, encode)
import Generators ()
import Model.Beer
import Model.BeerStyle
import Test.QuickCheck
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Text.RawString.QQ

test_validBeerWithoutAlcoholRateAsJson =
  testCase "When decoding a valid JSON without alcohol rate, it succeeds" $
  let Just beer :: Maybe Beer =
        decode
          [r|
{
  "id": 1,
  "name": "Brewdog IPA",
  "style": "INDIA_PALE_ALE"
}
        |]
   in beer @?= Beer {id = 1, name = "Brewdog IPA", style = IndiaPaleAle, alcohol = Nothing}

test_validBeerWithAlcoholRateAsJson =
  testCase "When decoding a valid JSON with alcohol rate, it succeeds" $
  let Just beer :: Maybe Beer =
        decode
          [r|
{
  "id": 1,
  "name": "Brewdog IPA",
  "style": "INDIA_PALE_ALE",
  "alcohol": 5.4
}
        |]
   in beer @?= Beer {id = 1, name = "Brewdog IPA", style = IndiaPaleAle, alcohol = Just 5.4}
--
--test_jsonInverse =
--  testProperty "When encoding and decoding an beer, it returns the original beer" $ \(anyBeer :: Beer) ->
--    Just anyBeer === decode (encode anyBeer)
--
--test_jsonInjective =
--  testProperty "When encoding different beers, the JSONs are different" $ \(beer1 :: Beer) (beer2 :: Beer) ->
--    beer1 /= beer2 ==> encode beer1 =/= encode beer2
