{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Foreign.Callback (
  Callback(..),
  FunPtr
) where

import           Prelude
import           Foreign.Ptr

class Callback from to where
  wrapF :: from -> IO (FunPtr to)
