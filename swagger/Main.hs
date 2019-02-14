{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
module Main where

import Api (Api)
import Data.Aeson (encode)
import Servant (Proxy (..))
import Servant.API ((:>))
import Servant.Checked.Exceptions (ErrStatus, Throws)
import Servant.Checked.Exceptions.Extra (ErrDescription)
import Servant.Swagger (HasSwagger, toSwagger)

import qualified Data.ByteString.Lazy.Char8 as BLC8

-- | For the moment, ignore the @'Throws'@ combinator.
--
-- Ultimately, we would like to add an appropriate response section in the
-- generated Swagger output for each instance of @'Throws'@.
instance (HasSwagger sub, ErrDescription err, ErrStatus err) =>
    HasSwagger (Throws err :> sub) where
  toSwagger _ = toSwagger (Proxy :: Proxy sub)

main :: IO ()
main = BLC8.putStrLn $ encode $ toSwagger (Proxy :: Proxy Api)

