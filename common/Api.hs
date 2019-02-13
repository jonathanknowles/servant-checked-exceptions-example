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
  , DuplicateLocationNameError (..)
  , EmptyLocationNameError (..)
  , NegativeLocationIdError (..)
  , NoMatchingLocationError (..)
  ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Swagger (ToSchema)
import Data.Text (Text)
import GHC.Generics (Generic)
import Servant (JSON, Summary, Proxy (..))
import Servant.API ((:<|>) (..), (:>), Capture, Get)
import Servant.Checked.Exceptions (ErrStatus (..), Throws)

type Api = AddLocation :<|> FindLocationById :<|> FindLocationByName

api :: Proxy Api
api = Proxy

type AddLocation = "location"
  :> "add"
  :> Summary "Add a new location"
  :> Capture "locationName" Text
  :> Throws DuplicateLocationNameError
  :> Throws EmptyLocationNameError
  :> Get '[JSON] Location

type FindLocationById = "location"
  :> "findById"
  :> Summary "Find a location by ID"
  :> Capture "locationId" Integer
  :> Throws NegativeLocationIdError
  :> Throws NoMatchingLocationError
  :> Get '[JSON] Location

type FindLocationByName = "location"
  :> "findByName"
  :> Summary "Find a location by name"
  :> Capture "locationName" Text
  :> Throws EmptyLocationNameError
  :> Throws NoMatchingLocationError
  :> Get '[JSON] Location

data Location = Location
  { locationId   :: Integer
  , locationName :: Text
  } deriving (Eq, Generic, Show, FromJSON, ToJSON, ToSchema)

data DuplicateLocationNameError = DuplicateLocationNameError
  deriving (Eq, Generic, Show, FromJSON, ToJSON)
data EmptyLocationNameError = EmptyLocationNameError
  deriving (Eq, Generic, Show, FromJSON, ToJSON)
data NegativeLocationIdError = NegativeLocationIdError
  deriving (Eq, Generic, Show, FromJSON, ToJSON)
data NoMatchingLocationError = NoMatchingLocationError
  deriving (Eq, Generic, Show, FromJSON, ToJSON)

instance ErrStatus DuplicateLocationNameError where
  toErrStatus _ = toEnum 400
instance ErrStatus EmptyLocationNameError where
  toErrStatus _ = toEnum 400
instance ErrStatus NegativeLocationIdError where
  toErrStatus _ = toEnum 400
instance ErrStatus NoMatchingLocationError where
  toErrStatus _ = toEnum 404

class ErrDescription e where
  toErrDescription :: e -> Text
instance ErrDescription DuplicateLocationNameError where
  toErrDescription _ = "The specified location name already exists."
instance ErrDescription EmptyLocationNameError where
  toErrDescription _ = "The specified location name is empty."
instance ErrDescription NegativeLocationIdError where
  toErrDescription _ = "A location ID cannot be negative."
instance ErrDescription NoMatchingLocationError where
  toErrDescription _ = "A matching location was not found."

