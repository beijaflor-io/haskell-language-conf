module Data.Conf.Aeson
  where

import           Data.Aeson
import           Data.Conf.Types

fromToJSON :: ToJSON a => a -> Result Conf
fromToJSON = fromJSON . toJSON
