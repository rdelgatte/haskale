module Model.BeerSpec where

import Data.Aeson (decode, encode)
import Data.Aeson.Encode.Pretty
import Data.ByteString.Lazy (ByteString)
import Generators ()
import Model.Beer
import Model.BeerStyle
import Test.QuickCheck
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Text.RawString.QQ

-- So we can make sure the object is encoding with the expected key ordering (as we assert results as ByteString)
encodePrettyOptions :: Config
encodePrettyOptions = Config (Spaces 2) compare Generic False

beerWithoutAlcoholRateAsJson :: ByteString
beerWithoutAlcoholRateAsJson =
  [r|{
  "alcohol": null,
  "id": null,
  "name": "Brewdog IPA",
  "style": "INDIA_PALE_ALE"
}|]

beerWithoutAlcoholRate :: Beer
beerWithoutAlcoholRate = Beer {id = Nothing, name = "Brewdog IPA", style = IndiaPaleAle, alcohol = Nothing}

beerWithAlcoholRateAsJson :: ByteString
beerWithAlcoholRateAsJson =
  [r|{
  "alcohol": 5.4,
  "id": 1,
  "name": "Brewdog IPA",
  "style": "INDIA_PALE_ALE"
}|]

beerWithAlcoholRate :: Beer
beerWithAlcoholRate = Beer {id = Just 1, name = "Brewdog IPA", style = IndiaPaleAle, alcohol = Just 5.4}

test_encodeValidBeerWithoutAlcoholRate =
  testCase "When encoding a beer without alcohol rate, it returns a JSON" $
  encodePretty' encodePrettyOptions beerWithoutAlcoholRate @?= beerWithoutAlcoholRateAsJson

test_encodeValidBeerWithAlcoholRate =
  testCase "When encoding a beer with alcohol rate, it returns a JSON" $
  encodePretty' encodePrettyOptions beerWithAlcoholRate @?= beerWithAlcoholRateAsJson
--test_decodeValidBeerWithoutAlcoholRateAsJson =
--  testCase "When decoding a valid JSON without alcohol rate, it succeeds" $
--  let Just beer :: Maybe Beer = decode beerWithoutAlcoholRateAsJson
--   in beer @?= beerWithoutAlcoholRate
--
--test_decodeValidBeerWithAlcoholRateAsJson =
--  testCase "When decoding a valid JSON with alcohol rate, it succeeds" $
--  let Just beer :: Maybe Beer = decode beerWithAlcoholRateAsJson
--   in beer @?= beerWithAlcoholRate
--
--test_jsonInverse =
--  testProperty "When encoding and decoding an beer, it returns the original beer" $ \(anyBeer :: Beer) ->
--    Just anyBeer === decode (encode anyBeer)
--
--test_jsonInjective =
--  testProperty "When encoding different beers, the JSONs are different" $ \(beer1 :: Beer) (beer2 :: Beer) ->
--    beer1 /= beer2 ==> encode beer1 =/= encode beer2
