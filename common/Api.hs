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

import Data.Aeson (FromJSON, ToJSON)
import Data.Swagger (ToSchema)
import Data.Text (Text)
import GHC.Generics (Generic)
import Servant (JSON, Summary, Proxy (..))
import Servant.API ((:<|>) (..), (:>), Capture, Get, Put)
import Servant.Checked.Exceptions (ErrStatus (..), Throws)
import Servant.Checked.Exceptions.Extra (ErrDescription (..))

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
  deriving (Eq, Generic, Show, FromJSON, ToJSON)
data LocationNameHasInvalidCharsError = LocationNameHasInvalidCharsError
  deriving (Eq, Generic, Show, FromJSON, ToJSON)
data NoMatchingLocationError = NoMatchingLocationError
  deriving (Eq, Generic, Show, FromJSON, ToJSON)

instance ErrStatus LocationNameHasInvalidCharsError where
  toErrStatus _ = toEnum 400
instance ErrStatus LocationNameTooShortError where
  toErrStatus _ = toEnum 400
instance ErrStatus NoMatchingLocationError where
  toErrStatus _ = toEnum 404

instance ErrDescription LocationNameHasInvalidCharsError where
  toErrDescription _ =
    "A location name can only contain alphabetic and space characters."
instance ErrDescription LocationNameTooShortError where
  toErrDescription _ =
    "The specified location name was too short."
instance ErrDescription NoMatchingLocationError where
  toErrDescription _ =
    "A matching location was not found."

locationNameTooShort :: Text -> Bool
locationNameTooShort = (< minimumLocationNameLength) . Text.length

minimumLocationNameLength :: Int
minimumLocationNameLength = 2

locationNameHasInvalidChars :: Text -> Bool
locationNameHasInvalidChars = Text.any (not . isCharValidForLocationName)

isCharValidForLocationName :: Char -> Bool
isCharValidForLocationName c = Char.isAlpha c || Char.isSpace c

