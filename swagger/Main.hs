{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
module Main where

import Api (Api)
import Control.Lens ((.~), (%~))
import Data.Aeson (encode)
import Data.Function ((&))
import Data.Swagger (description)
import Data.Swagger.Operation (setResponseWith)
import Servant (Proxy (..))
import Servant.API ((:>))
import Servant.Checked.Exceptions (ErrStatus (..), Throws)
import Servant.Checked.Exceptions.Extra (ErrDescription (..))
import Servant.Swagger (HasSwagger, toSwagger)

import qualified Data.ByteString.Lazy.Char8 as BLC8

type IsErr err = (ErrDescription err, ErrStatus err)

-- See the following link for further details:
-- https://github.com/haskell-servant/servant-swagger/issues/59
instance (IsErr err, HasSwagger sub) => HasSwagger (Throws err :> sub)
  where
    toSwagger _ =
      toSwagger (Proxy :: Proxy sub) &
        setResponseWith
          (\old _ -> addDescription old)
          (fromEnum $ toErrStatus (undefined :: err))
          (return $ mempty & description .~ errDescription)
        where
          addDescription = description %~ ((errDescription <> " OR ") <>)
          errDescription = toErrDescription (undefined :: err)

main :: IO ()
main = BLC8.putStrLn $ encode $ toSwagger (Proxy :: Proxy Api)

