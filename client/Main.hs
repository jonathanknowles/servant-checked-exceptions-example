{-# LANGUAGE DataKinds #-}

module Main where

import Api (InvalidLocationError, NoSuchLocationError, Location, api)

import Config (baseUrl)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (void)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Servant.Client (ClientM (..), client, mkClientEnv, runClientM)
import Servant.Checked.Exceptions (Envelope, catchesEnvelope)

getLocationById
  :: Integer
  -> ClientM (Envelope '[InvalidLocationError, NoSuchLocationError] Location)
getLocationById = client api

main :: IO ()
main = do
  manager <- newManager defaultManagerSettings
  let env = mkClientEnv manager baseUrl
  print =<< runClientM program env

program :: ClientM ()
program = do
  liftIO $ putStrLn "Enter the ID of a location (non-negative integer):"
  locationId <- liftIO readLn
  result <- getLocationById locationId
  liftIO $ do
    putStrLn "Result:"
    putStrLn $ catchesEnvelope (show, show) show result

