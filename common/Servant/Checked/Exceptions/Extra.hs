module Servant.Checked.Exceptions.Extra where

import Data.Text (Text)

class ErrDescription e where
  toErrDescription :: e -> Text
