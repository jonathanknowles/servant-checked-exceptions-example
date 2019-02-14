module Main where

import Config (baseUrl)
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Servant.Client (ClientM (..), mkClientEnv, runClientM)
import Servant.Checked.Exceptions (catchesEnvelope)
import System.Environment (getArgs)

import qualified Client
import qualified Data.Text as Text

main :: IO ()
main = do
  args <- getArgs
  let program = case args of
        ["addLocation", name] ->
          addLocation (Text.pack name)
        ["findLocationById", key] ->
          findLocationById (read key)
        ["findLocationByName", name] ->
          findLocationByName (Text.pack name)
  manager <- newManager defaultManagerSettings
  let env = mkClientEnv manager baseUrl
  print =<< runClientM program env

  where

    addLocation :: Text -> ClientM ()
    addLocation name = do
      result <- Client.addLocation name
      liftIO $ putStrLn $ catchesEnvelope (show, show) show result

    findLocationById :: Integer -> ClientM ()
    findLocationById key = do
      result <- Client.findLocationById key
      liftIO $ putStrLn $ catchesEnvelope show show result

    findLocationByName :: Text -> ClientM ()
    findLocationByName name = do
      result <- Client.findLocationByName name
      liftIO $ putStrLn $ catchesEnvelope show show result

