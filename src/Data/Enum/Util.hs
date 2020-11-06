module Data.Enum.Util where

import           Prelude

mapEnum :: (Enum a, Enum b) => a -> b
mapEnum = toEnum . fromEnum
