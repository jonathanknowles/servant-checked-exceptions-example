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
import Data.IORef (IORef (..), newIORef, readIORef)
import Data.Map.Strict (Map)
import Data.Text (Text)
import Network.Wai.Handler.Warp (run)
import Servant (Handler, Server, serve)
import Servant.API ((:<|>) (..))
import Servant.Checked.Exceptions (Envelope, pureErrEnvelope, pureSuccEnvelope)

import qualified Data.ByteString.Lazy.Char8 as BSL8
import qualified Data.Map.Strict as Map

main :: IO ()
main = do
    locationMapRef <- newIORef locationMap
    run port $ serve api (server locationMapRef)
  where
    locationMap :: Map Integer Location
    locationMap = Map.fromDistinctAscList
      $ (\(i, n) -> (i, Location i n)) <$> zip [0 ..] locations

    locations :: [Text]
    locations =
      [ "Amsterdam"
      , "Berlin"
      , "Boston"
      , "Cambridge"
      , "Osaka"
      , "Paris"
      , "Stockholm"
      , "Taipei" ]

server :: IORef (Map Integer Location) -> Server Api
server locationMapRef = addLocation :<|> getLocationById

  where

    addLocation
      :: Text
      -> Handler (Envelope '[ DuplicateLocationNameError
                            , EmptyLocationNameError
                            ] Location)
    addLocation name = pureSuccEnvelope (Location 0 "Dummy")

    getLocationById
      :: Integer
      -> Handler (Envelope '[ NegativeLocationIdError
                            , NoMatchingLocationError
                            ] Location)
    getLocationById lid
      | lid < 0   = pureErrEnvelope NegativeLocationIdError
      | otherwise = do
          locationMap <- liftIO $ readIORef locationMapRef
          maybe (pureErrEnvelope NoMatchingLocationError)
                      pureSuccEnvelope $ Map.lookup lid locationMap

