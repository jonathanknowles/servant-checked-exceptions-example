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
import Servant.Checked.Exceptions.Extra (ErrDescription (..))

type Api = AddLocation :<|> FindLocationById :<|> FindLocationByName

api :: Proxy Api
api = Proxy

type AddLocation = "location"
  :> "add"
  :> Summary "Add a new location"
  :> Throws EmptyLocationNameError
  :> Capture "locationName" Text
  :> Get '[JSON] Location

type FindLocationById = "location"
  :> "findById"
  :> Summary "Find a location by ID"
  :> Throws NegativeLocationIdError
  :> Throws NoMatchingLocationError
  :> Capture "locationId" Integer
  :> Get '[JSON] Location

type FindLocationByName = "location"
  :> "findByName"
  :> Summary "Find a location by name"
  :> Throws EmptyLocationNameError
  :> Throws NoMatchingLocationError
  :> Capture "locationName" Text
  :> Get '[JSON] Location

data Location = Location
  { locationId   :: Integer
  , locationName :: Text
  } deriving (Eq, Generic, Show, FromJSON, ToJSON, ToSchema)

data EmptyLocationNameError = EmptyLocationNameError
  deriving (Eq, Generic, Show, FromJSON, ToJSON)
data NegativeLocationIdError = NegativeLocationIdError
  deriving (Eq, Generic, Show, FromJSON, ToJSON)
data NoMatchingLocationError = NoMatchingLocationError
  deriving (Eq, Generic, Show, FromJSON, ToJSON)

instance ErrStatus EmptyLocationNameError where
  toErrStatus _ = toEnum 400
instance ErrStatus NegativeLocationIdError where
  toErrStatus _ = toEnum 400
instance ErrStatus NoMatchingLocationError where
  toErrStatus _ = toEnum 404

instance ErrDescription EmptyLocationNameError where
  toErrDescription _ = "The specified location name is empty."
instance ErrDescription NegativeLocationIdError where
  toErrDescription _ = "A location ID cannot be negative."
instance ErrDescription NoMatchingLocationError where
  toErrDescription _ = "A matching location was not found."

