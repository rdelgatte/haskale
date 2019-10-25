module Json.Custom
  ( upperSnakeOptions
  , upperSnakeSchema
  ) where

import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Char
import           Data.Swagger

-- | Aeson options to convert a type (usually a sum type) between Haskell PascalCase and Aeson UPPER_SNAKE_CASE
--
-- @
-- data RoleName = Admin | Writer
--
-- instance ToJSON RoleName where
--   toJSON = genericToJSON upperSnakeOptions
-- instance FromJSON RoleName where
--   parseJSON = genericParseJSON upperSnakeOptions
-- @
--
-- >>> encode Admin
-- "ADMIN"
-- >>> encode Writer
-- "WRITER"
upperSnakeOptions :: Options
upperSnakeOptions = customDefaultOptions {constructorTagModifier = fmap toUpper . snakeCase}

upperSnakeSchema :: SchemaOptions
upperSnakeSchema = fromAesonOptions upperSnakeOptions

customDefaultOptions :: Options
customDefaultOptions = defaultOptions {omitNothingFields = True}
