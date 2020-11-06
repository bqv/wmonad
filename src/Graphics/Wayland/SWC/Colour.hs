{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Graphics.Wayland.SWC.Colour (
  Colour,
  rgbToC,
  rgbFromC
) where

import           Prelude
import           Control.Monad
import           Data.Binary.Get
import           Data.Binary.Put
import qualified Data.Colour.SRGB as RGB
import qualified Data.Colour.SRGB.Linear as LRGB
import           Data.Word
import           Foreign.Callable

type Colour = RGB.Colour Double
instance {-# OVERLAPPING #-} Arg Colour where
  arg = arg . LRGB.toRGB

getWord32 = getWord32be
putWord32 = putWord32be

rgbToC :: Colour -> Word32
rgbToC = word8To32 . rgbToWord8
  where
    rgbToWord8 :: Colour -> [Word8]
    rgbToWord8 = (\(RGB.RGB r g b) -> [0,r,g,b]) . RGB.toSRGB24
    word8To32 :: [Word8] -> Word32
    word8To32 = runGet getWord32 . runPut . mapM_ putWord8

rgbFromC :: Word32 -> Colour
rgbFromC = word8ToRGB . word32To8
  where
    word32To8 :: Word32 -> [Word8]
    word32To8 = runGet (replicateM 4 getWord8) . runPut . putWord32
    word8ToRGB :: [Word8] -> Colour
    word8ToRGB (_:r:g:b:[]) = RGB.sRGB24 r g b
