{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Api
  ( Api
  , api
  , Location (..)
  , AddLocation
  , FindLocationById
  , FindLocationByName
  , LocationNameHasInvalidCharsError (..)
  , locationNameHasInvalidChars
  , LocationNameTooShortError (..)
  , locationNameTooShort
  , NoMatchingLocationError (..)
  ) where

import Data.Aeson (FromJSON (..), ToJSON (..), withText)
import Data.Aeson.Types (Parser, Value)
import Data.Swagger (ToSchema)
import Data.Text (Text)
import GHC.Generics (Generic)
import Servant (JSON, Summary, Proxy (..))
import Servant.API ((:<|>) (..), (:>), Capture, Get, Put)
import Servant.Checked.Exceptions (ErrStatus (..), Throws)
import Servant.Checked.Exceptions.Extra (ErrDescription (..))
import Text.Read (readMaybe)

import qualified Data.Char as Char
import qualified Data.Text as Text

type Api = AddLocation :<|> FindLocationById :<|> FindLocationByName

api :: Proxy Api
api = Proxy

type AddLocation = "location"
  :> "add"
  :> Summary "Add a new location"
  :> Throws LocationNameHasInvalidCharsError
  :> Throws LocationNameTooShortError
  :> Capture "locationName" Text
  :> Put '[JSON] Location

type FindLocationById = "location"
  :> "findById"
  :> Summary "Find a location by ID"
  :> Throws NoMatchingLocationError
  :> Capture "locationId" Integer
  :> Get '[JSON] Location

type FindLocationByName = "location"
  :> "findByName"
  :> Summary "Find a location by name"
  :> Throws NoMatchingLocationError
  :> Capture "locationName" Text
  :> Get '[JSON] Location

data Location = Location
  { locationId   :: Integer
  , locationName :: Text
  } deriving (Eq, Generic, Show, FromJSON, ToJSON, ToSchema)

data LocationNameTooShortError = LocationNameTooShortError
  deriving (Eq, Generic, Read, Show)
data LocationNameHasInvalidCharsError = LocationNameHasInvalidCharsError
  deriving (Eq, Generic, Read, Show)
data NoMatchingLocationError = NoMatchingLocationError
  deriving (Eq, Generic, Read, Show)

instance ToJSON LocationNameTooShortError where
  toJSON = toJSON . show
instance ToJSON LocationNameHasInvalidCharsError where
  toJSON = toJSON . show
instance ToJSON NoMatchingLocationError where
  toJSON = toJSON . show

instance FromJSON LocationNameTooShortError where
  parseJSON = parseText LocationNameTooShortError
instance FromJSON LocationNameHasInvalidCharsError where
  parseJSON = parseText LocationNameHasInvalidCharsError
instance FromJSON NoMatchingLocationError where
  parseJSON = parseText NoMatchingLocationError

parseText :: Show a => Read a => a -> Value -> Parser a
parseText x = withText (show x) $
    maybe (fail (show x ++ " parse failure")) pure
      . readMaybe . Text.unpack

instance ErrStatus LocationNameHasInvalidCharsError where
  toErrStatus _ = toEnum 400
instance ErrStatus LocationNameTooShortError where
  toErrStatus _ = toEnum 400
instance ErrStatus NoMatchingLocationError where
  toErrStatus _ = toEnum 404

instance ErrDescription LocationNameHasInvalidCharsError where
  toErrDescription _ =
    "the location name contained non-alphabetic characters"
instance ErrDescription LocationNameTooShortError where
  toErrDescription _ =
    "the location name was too short"
instance ErrDescription NoMatchingLocationError where
  toErrDescription _ =
    "a matching location was not found"

locationNameTooShort :: Text -> Bool
locationNameTooShort = (< minimumLocationNameLength) . Text.length

minimumLocationNameLength :: Int
minimumLocationNameLength = 2

locationNameHasInvalidChars :: Text -> Bool
locationNameHasInvalidChars = Text.any (not . isCharValidForLocationName)

isCharValidForLocationName :: Char -> Bool
isCharValidForLocationName c = Char.isAlpha c || Char.isSpace c

