{-# LANGUAGE DataKinds #-}

module Main where

import Api
        ( Api, api, Location (..)
        , DuplicateLocationNameError (..)
        , EmptyLocationNameError (..)
        , NegativeLocationIdError (..)
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
  -> ClientM (Envelope '[ DuplicateLocationNameError
                        , EmptyLocationNameError
                        ] Location)

getLocationById
  :: Integer
  -> ClientM (Envelope '[ NegativeLocationIdError
                        , NoMatchingLocationError] Location)

addLocation :<|> getLocationById = client api

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

