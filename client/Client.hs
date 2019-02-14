{-# LANGUAGE DataKinds #-}

module Client
  ( addLocation
  , findLocationById
  , findLocationByName
  ) where

import Api
        ( Api, api
        , Location (..)
        , LocationNameHasInvalidCharsError (..)
        , LocationNameTooShortError (..)
        , NoMatchingLocationError (..) )
import Data.Text (Text)
import Servant.API ((:<|>) (..))
import Servant.Client (ClientM (..), client)
import Servant.Checked.Exceptions (Envelope)

addLocation
  :: Text
  -> ClientM (Envelope '[ LocationNameHasInvalidCharsError
                        , LocationNameTooShortError
                        ] Location)

findLocationById
  :: Integer
  -> ClientM (Envelope '[ NoMatchingLocationError] Location)

findLocationByName
  :: Text
  -> ClientM (Envelope '[ NoMatchingLocationError] Location)

addLocation
  :<|> findLocationById
  :<|> findLocationByName = client api

