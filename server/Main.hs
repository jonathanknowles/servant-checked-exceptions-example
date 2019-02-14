{-# LANGUAGE DataKinds #-}

module Main where

import Api
        ( Api, api, Location (..)
        , LocationNameHasInvalidCharsError (..)
        , locationNameHasInvalidChars
        , LocationNameTooShortError (..)
        , locationNameTooShort
        , NoMatchingLocationError (..) )
import Config (port)
import Control.Monad.IO.Class (liftIO)
import Data.IORef (IORef (..), newIORef, readIORef, writeIORef)
import Data.Text (Text)
import LocationMap (LocationMap)
import Network.Wai.Handler.Warp (run)
import Servant (Handler, Server, serve)
import Servant.API ((:<|>) (..))
import Servant.Checked.Exceptions (Envelope, pureErrEnvelope, pureSuccEnvelope)

import qualified LocationMap
import qualified Data.Text as Text

main :: IO ()
main = do
  locationMapRef <- newIORef LocationMap.empty
  run port $ serve api (server locationMapRef)

server :: IORef LocationMap -> Server Api
server locationMapRef =
    addLocation :<|> findLocationById :<|> findLocationByName

  where

    addLocation
      :: Text
      -> Handler (Envelope '[ LocationNameHasInvalidCharsError
                            , LocationNameTooShortError
                            ] Location)
    addLocation name
      | locationNameHasInvalidChars name =
          pureErrEnvelope LocationNameHasInvalidCharsError
      | locationNameTooShort name =
          pureErrEnvelope LocationNameTooShortError
      | otherwise = do
          locationMap <- liftIO $ readIORef locationMapRef
          let (locationMap', location') = LocationMap.add locationMap name
          liftIO $ writeIORef locationMapRef locationMap'
          pureSuccEnvelope location'

    findLocationById
      :: Integer
      -> Handler (Envelope '[ NoMatchingLocationError ] Location)
    findLocationById key = do
      locationMap <- liftIO $ readIORef locationMapRef
      maybe (pureErrEnvelope NoMatchingLocationError)
        pureSuccEnvelope $ LocationMap.findById locationMap key

    findLocationByName
      :: Text
      -> Handler (Envelope '[ NoMatchingLocationError ] Location)
    findLocationByName key = do
      locationMap <- liftIO $ readIORef locationMapRef
      maybe (pureErrEnvelope NoMatchingLocationError)
        pureSuccEnvelope $ LocationMap.findByName locationMap key

