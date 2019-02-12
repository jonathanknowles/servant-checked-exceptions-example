{-# LANGUAGE DataKinds #-}

module Main where

import Api (InvalidLocationError, NoSuchLocationError, Location, api)

import Config (baseUrl)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Servant.Client (ClientM (..), client, mkClientEnv, runClientM)
import Servant.Checked.Exceptions (Envelope)

getLocationById
  :: Integer
  -> ClientM (Envelope '[InvalidLocationError, NoSuchLocationError] Location)
getLocationById = client api

main :: IO ()
main = do
  manager <- newManager defaultManagerSettings
  let env = mkClientEnv manager baseUrl
  putStrLn "Enter a location ID:"
  locationId <- readLn
  print =<< runClientM (getLocationById locationId) env

