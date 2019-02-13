{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Api
        ( Api, api, Location (..)
        , DuplicateLocationNameError (..)
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

import qualified LocationMap as LocationMap
import qualified Data.ByteString.Lazy.Char8 as BSL8

main :: IO ()
main = do
  locationMapRef <- newIORef LocationMap.empty
  run port $ serve api (server locationMapRef)

server :: IORef LocationMap -> Server Api
server locationMapRef =
    serverAddLocation :<|> serverFindLocationById :<|> serverFindLocationByName

  where

    serverAddLocation
      :: Text
      -> Handler (Envelope '[ DuplicateLocationNameError
                            , EmptyLocationNameError
                            ] Location)
    serverAddLocation name = do
      locationMap <- liftIO $ readIORef locationMapRef
      if name == ""
      then pureErrEnvelope EmptyLocationNameError
      else
        if LocationMap.containsName locationMap name
        then pureErrEnvelope DuplicateLocationNameError
        else do
          let (locationMap', location') = LocationMap.add locationMap name
          liftIO $ writeIORef locationMapRef locationMap'
          pureSuccEnvelope location'

    serverFindLocationById
      :: Integer
      -> Handler (Envelope '[ NegativeLocationIdError
                            , NoMatchingLocationError
                            ] Location)
    serverFindLocationById key
      | key < 0   = pureErrEnvelope NegativeLocationIdError
      | otherwise = do
          locationMap <- liftIO $ readIORef locationMapRef
          maybe (pureErrEnvelope NoMatchingLocationError)
            pureSuccEnvelope $ LocationMap.findById locationMap key

    serverFindLocationByName
      :: Text
      -> Handler (Envelope '[ EmptyLocationNameError
                            , NoMatchingLocationError
                            ] Location)
    serverFindLocationByName key
      | key == "" = pureErrEnvelope EmptyLocationNameError
      | otherwise = do
          locationMap <- liftIO $ readIORef locationMapRef
          maybe (pureErrEnvelope NoMatchingLocationError)
            pureSuccEnvelope $ LocationMap.findByName locationMap key

