{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

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
import Data.Map.Strict (Map)
import Data.Text (Text)
import Network.Wai.Handler.Warp (run)
import Servant (Handler, Server, serve)
import Servant.API ((:<|>) (..))
import Servant.Checked.Exceptions (Envelope, pureErrEnvelope, pureSuccEnvelope)

import qualified Data.ByteString.Lazy.Char8 as BSL8
import qualified Data.Map.Strict as Map

data LocationMap = LocationMap
  { locationIdToNameMap :: Map Integer Text
  , locationNameToIdMap :: Map Text Integer
  }

emptyLocationMap :: LocationMap
emptyLocationMap = LocationMap mempty mempty

isLocationMapEmpty :: LocationMap -> Bool
isLocationMapEmpty map = Map.null (locationIdToNameMap map)

nextLocationId :: LocationMap -> Integer
nextLocationId map = maybe 0 (succ . fst . fst) $
  Map.maxViewWithKey (locationIdToNameMap map)

findLocationById :: LocationMap -> Integer -> Maybe Location
findLocationById map key = Location key <$>
  Map.lookup key (locationIdToNameMap map)

findLocationByName :: LocationMap -> Text -> Maybe Location
findLocationByName map key = flip Location key <$>
  Map.lookup key (locationNameToIdMap map)

locationIdExists :: LocationMap -> Integer -> Bool
locationIdExists map key = Map.member key (locationIdToNameMap map)

locationNameExists :: LocationMap -> Text -> Bool
locationNameExists map key = Map.member key (locationNameToIdMap map)

addLocation :: LocationMap -> Text -> (LocationMap, Location)
addLocation map name' =
    maybe (map', location') (map, ) $ findLocationByName map name'
  where
    id' = nextLocationId map
    location' = Location id' name'
    locationIdToNameMap' = Map.insert id' name' $ locationIdToNameMap map
    locationNameToIdMap' = Map.insert name' id' $ locationNameToIdMap map
    map' = LocationMap locationIdToNameMap' locationNameToIdMap'

main :: IO ()
main = do
  locationMapRef <- newIORef emptyLocationMap
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
        if locationNameExists locationMap name
        then pureErrEnvelope DuplicateLocationNameError
        else do
          let (locationMap', location') = addLocation locationMap name
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
            pureSuccEnvelope $ findLocationById locationMap key

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
            pureSuccEnvelope $ findLocationByName locationMap key

