{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Api
  ( Api
  , api
  , GetLocationById
  , Location (..)
  , InvalidLocationError (..)
  , NoSuchLocationError (..)
  ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)
import Servant (JSON, Summary, Proxy (..))
import Servant.API ((:<|>) (..), (:>), Capture, Get)
import Servant.Checked.Exceptions (ErrStatus (..), Throws)

type Api = GetLocationById

api :: Proxy Api
api = Proxy

type GetLocationById = "location"
  :> Summary "Get a location by ID"
  :> Capture "locationId" Integer
  :> Throws InvalidLocationError
  :> Throws NoSuchLocationError
  :> Get '[JSON] Location

data Location = Location
  { locationId   :: Integer
  , locationName :: Text
  } deriving (Eq, Generic, Show, FromJSON, ToJSON)

data InvalidLocationError = InvalidLocationError
  deriving (Eq, Generic, Show, FromJSON, ToJSON)
data NoSuchLocationError = NoSuchLocationError
  deriving (Eq, Generic, Show, FromJSON, ToJSON)

instance ErrStatus InvalidLocationError where
  toErrStatus _ = toEnum 400
instance ErrStatus NoSuchLocationError where
  toErrStatus _ = toEnum 404

class ErrDescription e where
  toErrDescription :: e -> Text
instance ErrDescription InvalidLocationError where
  toErrDescription _ = "The specified location ID is invalid."
instance ErrDescription NoSuchLocationError where
  toErrDescription _ = "A location with the specified ID was not found."

