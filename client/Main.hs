{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Api
        ( Api, api
        , Location (..)
        , LocationNameHasInvalidCharsError (..)
        , LocationNameTooShortError (..)
        , NoMatchingLocationError (..) )
import Config (baseUrl)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (void)
import Data.Text (Text)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Servant.API ((:<|>) (..))
import Servant.Client (ClientM (..), client, mkClientEnv, runClientM)
import Servant.Checked.Exceptions (Envelope, catchesEnvelope)

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

addLocation :<|> findLocationById :<|> findLocationByName = client api

main :: IO ()
main = do
  manager <- newManager defaultManagerSettings
  let env = mkClientEnv manager baseUrl
  print =<< runClientM program env

program :: ClientM ()
program = do
  liftIO $ putStrLn "Adding locations..."
  mapM_ addLocation locations
  liftIO $ putStrLn "Enter the ID of a location (non-negative integer):"
  locationId <- liftIO readLn
  result <- findLocationById locationId
  liftIO $ do
    putStrLn "Result:"
    putStrLn $ catchesEnvelope show show result

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

