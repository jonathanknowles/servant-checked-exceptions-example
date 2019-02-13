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
import Servant.Swagger (HasSwagger, toSwagger)

import qualified Data.ByteString.Lazy.Char8 as BLC8

-- | The @'Throws'@ combinator currently does not change our specification.
instance (HasSwagger sub) => HasSwagger (Throws err :> sub) where
  toSwagger _ = toSwagger (Proxy :: Proxy sub)

main :: IO ()
main = BLC8.putStrLn $ encode $ toSwagger (Proxy :: Proxy Api)

