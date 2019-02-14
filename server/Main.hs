{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Api
        ( Api, api, Location (..)
        , EmptyLocationNameError (..)
        , NegativeLocationIdError (..)
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
      -> Handler (Envelope '[ EmptyLocationNameError
                            ] Location)
    addLocation name = do
      locationMap <- liftIO $ readIORef locationMapRef
      if name == ""
      then pureErrEnvelope EmptyLocationNameError
      else do
        let (locationMap', location') = LocationMap.add locationMap name
        liftIO $ writeIORef locationMapRef locationMap'
        pureSuccEnvelope location'

    findLocationById
      :: Integer
      -> Handler (Envelope '[ NegativeLocationIdError
                            , NoMatchingLocationError
                            ] Location)
    findLocationById key
      | key < 0   = pureErrEnvelope NegativeLocationIdError
      | otherwise = do
          locationMap <- liftIO $ readIORef locationMapRef
          maybe (pureErrEnvelope NoMatchingLocationError)
            pureSuccEnvelope $ LocationMap.findById locationMap key

    findLocationByName
      :: Text
      -> Handler (Envelope '[ EmptyLocationNameError
                            , NoMatchingLocationError
                            ] Location)
    findLocationByName key
      | key == "" = pureErrEnvelope EmptyLocationNameError
      | otherwise = do
          locationMap <- liftIO $ readIORef locationMapRef
          maybe (pureErrEnvelope NoMatchingLocationError)
            pureSuccEnvelope $ LocationMap.findByName locationMap key

