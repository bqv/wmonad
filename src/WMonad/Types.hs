{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
module WMonad.Types (
  locus, focus, store, focused,
  Core(),
  eventLoop, display, ffiThread,
  Display(),
  wlDisplay, screens,
  Screen(),
--windows, windowCount,
  Window(),
  currentScreen,
  module WMonad.Types
) where

import           Protolude hiding (to)
import           Data.Default
import qualified Data.Map as M
import           Control.Lens
import           Control.Lens.Prism
import           Foreign.Ptr (nullPtr)
import           Foreign.Storable (peek)
import qualified Graphics.Wayland.Server as WL
import qualified WMonad.SWC as SWC
import           WMonad.Types.Internal

peeked :: Storable a => Getter (Ptr a) (IO a)
peeked = to peek

type family CCtx r where
  CCtx Screen = SWC.Screen
  CCtx Window = SWC.Window

type Ctx r = StoreMap (CCtx r) r
type CtxPtr r = Ptr (CCtx r)

instance (c ~ Ctx r) => Default c where
  def = Context (uncurry . flip $ (sequence .) . liftM2 (liftA2 (,)) const M.lookup) $ (M.empty, nullPtr)
