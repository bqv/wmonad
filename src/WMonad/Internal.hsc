{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
module WMonad.Internal where

import qualified Data.Map as Map
import           Data.Word
import           Foreign.C.Types
import           Foreign.Ptr
import           Foreign.Storable
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Types as C
import qualified Language.Haskell.TH as TH

{- Context -}
wmCtx :: C.Context
wmCtx = C.baseCtx <> C.fptrCtx <> C.funCtx <> C.vecCtx <> C.bsCtx <> swcCtx
  where
    swcCtx = mempty { C.ctxTypesTable = wmTypesTable }

wmTypesTable :: Map.Map C.TypeSpecifier TH.TypeQ
wmTypesTable = Map.fromList [
--(C.TypeName "screen", [t| Screen |]) ,
  (C.Struct "swc_screen", [t| SwcScreen |]) ,
  (C.Struct "swc_window", [t| SwcWindow |]) ,
  (C.Struct "swc_screen_handler", [t| SwcScreenHandler |]) ,
  (C.Struct "swc_window_handler", [t| SwcWindowHandler |]) ,
  (C.Struct "swc_manager", [t| SwcManager |]) ,
  (C.Struct "wl_display", [t| WlDisplay |]) ,
  (C.Struct "wl_event_loop", [t| WlEventLoop |]) ]

{- Types -}
type NewScreenCallback = Ptr SwcScreen -> IO ()
type NewWindowCallback = Ptr SwcWindow -> IO ()

type SwcBindingCallback = Ptr () -> Word32 -> Word32 -> Word32 -> IO ()
type SwcDataCallback = Ptr () -> IO ()

data SwcScreen
data SwcWindow

data SwcScreenHandler = SwcScreenHandler {
  fnUsableGeometryChanged :: FunPtr SwcDataCallback,
  fnScreenEntered :: FunPtr SwcDataCallback
             -- .usable_geometry_changed = &screen_usable_geometry_changed,
             -- .entered = &screen_entered,
}

instance Storable SwcScreenHandler where
  sizeOf ~(SwcScreenHandler ugc se) = sizeOf ugc + sizeOf se
  alignment ~(SwcScreenHandler _ _) = alignment (undefined :: CInt)
  peek p                            = do
    ugc <- peek (castPtr p)
    se <- peekByteOff (castPtr p) (sizeOf ugc)
    return $ SwcScreenHandler ugc se
  poke p (SwcScreenHandler ugc se)  = do
    poke (castPtr p) ugc
    pokeByteOff (castPtr p) (sizeOf ugc) se

data SwcWindowHandler = SwcWindowHandler {
  fnScreenDestroy :: FunPtr SwcDataCallback,
  fnScreenEntered :: FunPtr SwcDataCallback
             -- .destroy = &window_destroy,
             -- .entered = &window_entered,
}

instance Storable SwcWindowHandler where
  sizeOf ~(SwcWindowHandler sd se)  = sizeOf sd + sizeOf se
  alignment ~(SwcWindowHandler _ _) = alignment (undefined :: CInt)
  peek p                            = do
    sd <- peek (castPtr p)
    se <- peekByteOff (castPtr p) (sizeOf sd)
    return $ SwcWindowHandler sd se
  poke p (SwcWindowHandler sd se)   = do
    poke (castPtr p) sd
    pokeByteOff (castPtr p) (sizeOf sd) se

data SwcManager = SwcManager {
  fnNewScreen :: FunPtr NewScreenCallback,
  fnNewWindow :: FunPtr NewWindowCallback
}

instance Storable SwcManager where
  sizeOf ~(SwcManager ns nw)  = sizeOf ns + sizeOf nw
  alignment ~(SwcManager _ _) = alignment (undefined :: CInt)
  peek p                      = do
    ns <- peek (castPtr p)
    nw <- peekByteOff (castPtr p) (sizeOf ns)
    return $ SwcManager ns nw
  poke p (SwcManager ns nw)   = do
    poke (castPtr p) ns
    pokeByteOff (castPtr p) (sizeOf ns) nw

data WlDisplay
data WlEventLoop

{- Constants -}
#include <wayland-server-protocol.h>
data WlKeyboardKeyState = WlKeyboardKeyStatePressed
                        | WlKeyboardKeyStateReleased
                        deriving (Eq, Show)

instance Enum WlKeyboardKeyState where
  fromEnum WlKeyboardKeyStatePressed = #{const WL_KEYBOARD_KEY_STATE_PRESSED}
  fromEnum WlKeyboardKeyStateReleased = #{const WL_KEYBOARD_KEY_STATE_RELEASED}
  toEnum #{const WL_KEYBOARD_KEY_STATE_PRESSED} = WlKeyboardKeyStatePressed
  toEnum #{const WL_KEYBOARD_KEY_STATE_RELEASED} = WlKeyboardKeyStateReleased
  toEnum _ = error "Bad value for WlKeyboardKeyState"
