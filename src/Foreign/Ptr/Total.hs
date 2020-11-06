module Foreign.Ptr.Total where

import           Prelude
import           Foreign.C.Types
import           Foreign.Ptr

safeCastPtr :: Ptr a -> Maybe (Ptr b)
safeCastPtr = fmap castPtr . safePtr

safePtr :: Ptr a -> Maybe (Ptr a)
safePtr p | p == nullPtr = Nothing
safePtr p                = Just p
