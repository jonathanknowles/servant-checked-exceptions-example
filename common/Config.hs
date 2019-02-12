module Config
  ( host
  , port
  , scheme
  , baseUrl
  ) where

import Servant.Client (BaseUrl (..), Scheme (Http))

port :: Int
port = 8081

host :: String
host = "localhost"

scheme :: Scheme
scheme = Http

baseUrl :: BaseUrl
baseUrl = BaseUrl scheme host port ""
