module Control.Comonad.Store.Zipper.Circular where

import           Control.Comonad.Store.Zipper
import           Control.Comonad.Store

peekc :: Int -> Zipper t a -> a
peekc n z
  | total == 0     = peek n z
  | n > total - 1  = peekc (n - total) z
  | n < 0          = peekc (n + total) z
  | otherwise      = peek n z
  where
    total = size z

seekc :: Int -> Zipper t a -> Zipper t a
seekc n z
  | total == 0             = seek n z
  | pos z + n > total - 1  = seekc (n - total) z
  | pos z + n < 0          = seekc (n + total) z
  | otherwise              = seek n z
  where
    total = size z
