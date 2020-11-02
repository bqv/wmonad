{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
module SWC.Internal where

import qualified Data.Map as Map
import           Data.Word
import           Foreign.C.Types
import           Foreign.Ptr
import           Foreign.Storable
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Types as C
import qualified Language.Haskell.TH as TH
import qualified SWC.Wayland as WL

#include <swc.h>

{- Context -}
ctx :: C.Context
ctx = C.baseCtx <> C.fptrCtx <> C.funCtx <> C.vecCtx <> C.bsCtx <> libCtx
  where
    libCtx = mempty { C.ctxTypesTable = swcTypesTable }

swcTypesTable :: Map.Map C.TypeSpecifier TH.TypeQ
swcTypesTable = Map.fromList [
--(C.TypeName "screen", [t| Screen |]) ,
  (C.Struct "swc_rectangle", [t| Rectangle |]) ,
  (C.Struct "swc_screen", [t| Screen |]) ,
  (C.Struct "swc_window", [t| Window |]) ,
  (C.Struct "swc_screen_handler", [t| ScreenHandler |]) ,
  (C.Struct "swc_window_handler", [t| WindowHandler |]) ,
  (C.Struct "swc_manager", [t| Manager |]) ,
  (C.Struct "libinput_device", [t| InputDevice |]) ,
  (C.Struct "wl_display", [t| WL.Display |]) ,
  (C.Struct "wl_event_loop_signal_func", [t| WL.SignalFunc |]) ,
  (C.Struct "wl_event_source", [t| WL.EventSource |]) ,
  (C.Struct "wl_event_loop", [t| WL.EventLoop |]) ]

{- Types -}
data InputDevice

type NewScreenCallback = Ptr Screen -> IO ()
type NewWindowCallback = Ptr Window -> IO ()
type NewDeviceCallback = Ptr InputDevice -> IO ()
type SessionCallback = IO ()

type BindingCallback = Ptr () -> Word32 -> Word32 -> Word32 -> IO ()
type DataCallback = Ptr () -> IO ()

{- Structs -}
data Rectangle = Rectangle {
  posX :: Int,
  posY :: Int,
  sizeW :: Word,
  sizeH :: Word
} deriving (Eq, Show)

instance Storable Rectangle where
  sizeOf _                        = #{size struct swc_rectangle}
  alignment (Rectangle _ _ _ _)   = #{alignment struct swc_rectangle}
  peek p                              = do
    x <- #{peek struct swc_rectangle, x} p
    y <- #{peek struct swc_rectangle, y} p
    w <- #{peek struct swc_rectangle, width} p
    h <- #{peek struct swc_rectangle, height} p
    return $ Rectangle x y w h
  poke p (Rectangle x y w h) = do
    #{poke struct swc_rectangle, x} p $ x
    #{poke struct swc_rectangle, y} p $ y
    #{poke struct swc_rectangle, width} p $ w
    #{poke struct swc_rectangle, height} p $ h

data ScreenHandler = ScreenHandler {
  fnScreenDestroy :: FunPtr DataCallback,
  fnGeometryChanged :: FunPtr DataCallback,
  fnUsableGeometryChanged :: FunPtr DataCallback,
  fnScreenEntered :: FunPtr DataCallback
} deriving (Show)

instance Storable ScreenHandler where
  sizeOf _                            = #{size struct swc_screen_handler}
  alignment (ScreenHandler _ _ _ _)   = #{alignment struct swc_screen_handler}
  peek p                              = do
    sd <- #{peek struct swc_screen_handler, destroy} p
    gc <- #{peek struct swc_screen_handler, geometry_changed} p
    ugc <- #{peek struct swc_screen_handler, usable_geometry_changed} p
    se <- #{peek struct swc_screen_handler, entered} p
    return $ ScreenHandler sd gc ugc se
  poke p (ScreenHandler sd gc ugc se) = do
    #{poke struct swc_screen_handler, destroy} p $ sd
    #{poke struct swc_screen_handler, geometry_changed} p $ gc
    #{poke struct swc_screen_handler, usable_geometry_changed} p $ ugc
    #{poke struct swc_screen_handler, entered} p $ se

data Screen = Screen {
  screenGeometry :: Rectangle,
  screenUsableGeometry :: Rectangle
} deriving (Show)

instance Storable Screen where
  sizeOf _               = #{size struct swc_screen}
  alignment (Screen _ _) = #{alignment struct swc_screen}
  peek p                 = do
    g <- #{peek struct swc_screen, geometry} p
    ug <- #{peek struct swc_screen, usable_geometry} p
    return $ Screen g ug
  poke p (Screen g ug)   = do
    #{poke struct swc_screen, geometry} p $ g
    #{poke struct swc_screen, usable_geometry} p $ ug

data WindowHandler = WindowHandler {
  fnWindowDestroy :: FunPtr DataCallback,
  fnTitleChanged :: FunPtr DataCallback,
  fnAppIdChanged :: FunPtr DataCallback,
  fnParentChanged :: FunPtr DataCallback,
  fnWindowEntered :: FunPtr DataCallback,
  fnWindowMove :: FunPtr DataCallback,
  fnWindowResize :: FunPtr DataCallback
} deriving (Show)

instance Storable WindowHandler where
  sizeOf _                                     = #{size struct swc_window_handler}
  alignment (WindowHandler _ _ _ _ _ _ _)      = #{alignment struct swc_window_handler}
  peek p                                       = do
    wd <- #{peek struct swc_window_handler, destroy} p
    tc <- #{peek struct swc_window_handler, title_changed} p
    aic <- #{peek struct swc_window_handler, app_id_changed} p
    pc <- #{peek struct swc_window_handler, parent_changed} p
    we <- #{peek struct swc_window_handler, entered} p
    wm <- #{peek struct swc_window_handler, move} p
    wr <- #{peek struct swc_window_handler, resize} p
    return $ WindowHandler wd tc aic pc we wm wr
  poke p (WindowHandler wd tc aic pc we wm wr) = do
    #{poke struct swc_window_handler, destroy} p $ wd
    #{poke struct swc_window_handler, title_changed} p $ tc
    #{poke struct swc_window_handler, app_id_changed} p $ aic
    #{poke struct swc_window_handler, parent_changed} p $ pc
    #{poke struct swc_window_handler, entered} p $ we
    #{poke struct swc_window_handler, move} p $ wm
    #{poke struct swc_window_handler, resize} p $ wr

data Window = Window {
  windowTitle :: Ptr CChar,
  windowAppId :: Ptr CChar,
  windowParent :: Ptr Window
} deriving (Show)

instance Storable Window where
  sizeOf _                 = #{size struct swc_window}
  alignment (Window _ _ _) = #{alignment struct swc_window}
  peek p                   = do
    t <- #{peek struct swc_window, title} p
    ai <- #{peek struct swc_window, app_id} p
    par <- #{peek struct swc_window, parent} p
    return $ Window t ai par
  poke p (Window t ai par) = do
    #{poke struct swc_window, title} p $ t
    #{poke struct swc_window, app_id} p $ ai
    #{poke struct swc_window, parent} p $ par

data Manager = Manager {
  fnNewScreen :: FunPtr NewScreenCallback,
  fnNewWindow :: FunPtr NewWindowCallback,
  fnNewDevice :: FunPtr NewDeviceCallback,
  fnActivate :: FunPtr SessionCallback,
  fnDeactivate :: FunPtr SessionCallback
} deriving (Show)

instance Storable Manager where
  sizeOf _                      = #{size struct swc_manager}
  alignment (Manager _ _ _ _ _) = #{alignment struct swc_manager}
  peek p                        = do
    ns <- #{peek struct swc_manager, new_screen} p
    nw <- #{peek struct swc_manager, new_window} p
    nd <- #{peek struct swc_manager, new_device} p
    a <- #{peek struct swc_manager, activate} p
    d <- #{peek struct swc_manager, deactivate} p
    return $ Manager ns nw nd a d
  poke p (Manager ns nw nd a d) = do
    #{poke struct swc_manager, new_screen} p $ ns
    #{poke struct swc_manager, new_window} p $ nw
    #{poke struct swc_manager, new_device} p $ nd
    #{poke struct swc_manager, activate} p $ a
    #{poke struct swc_manager, deactivate} p $ d

{- Constants -}
data WindowEdge = EdgeAuto
                | EdgeTop
                | EdgeBottom
                | EdgeLeft
                | EdgeRight
                deriving (Eq, Show)

instance Enum WindowEdge where
  fromEnum EdgeAuto = #{const SWC_WINDOW_EDGE_AUTO}
  fromEnum EdgeTop = #{const SWC_WINDOW_EDGE_TOP}
  fromEnum EdgeBottom = #{const SWC_WINDOW_EDGE_BOTTOM}
  fromEnum EdgeLeft = #{const SWC_WINDOW_EDGE_LEFT}
  fromEnum EdgeRight = #{const SWC_WINDOW_EDGE_RIGHT}
  toEnum #{const SWC_WINDOW_EDGE_AUTO} = EdgeAuto
  toEnum #{const SWC_WINDOW_EDGE_TOP} = EdgeTop
  toEnum #{const SWC_WINDOW_EDGE_BOTTOM} = EdgeBottom
  toEnum #{const SWC_WINDOW_EDGE_LEFT} = EdgeLeft
  toEnum #{const SWC_WINDOW_EDGE_RIGHT} = EdgeRight
  toEnum _ = error "Bad value for SWC_WINDOW_EDGE_*"

data Modifier = ModifierCtrl
              | ModifierAlt
              | ModifierLogo
              | ModifierShift
              | ModifierAny
              | ModifierMask Int
              deriving (Eq, Show, Ord)

instance Enum Modifier where
  fromEnum ModifierCtrl = #{const SWC_MOD_CTRL}
  fromEnum ModifierAlt = #{const SWC_MOD_ALT}
  fromEnum ModifierLogo = #{const SWC_MOD_LOGO}
  fromEnum ModifierShift = #{const SWC_MOD_SHIFT}
  fromEnum ModifierAny = #{const SWC_MOD_ANY}
  fromEnum (ModifierMask n) = n
  toEnum #{const SWC_MOD_CTRL} = ModifierCtrl
  toEnum #{const SWC_MOD_ALT} = ModifierAlt
  toEnum #{const SWC_MOD_LOGO} = ModifierLogo
  toEnum #{const SWC_MOD_SHIFT} = ModifierShift
  toEnum n = ModifierMask n

instance Semigroup Modifier where
  (<>) a b = toEnum $ fromEnum a + fromEnum b

data BindingType = BindingKey
                 | BindingButton
                 deriving (Eq, Show, Ord)

instance Enum BindingType where
  fromEnum BindingKey = #{const SWC_BINDING_KEY}
  fromEnum BindingButton = #{const SWC_BINDING_BUTTON}
  toEnum #{const SWC_BINDING_KEY} = BindingKey
  toEnum #{const SWC_BINDING_BUTTON} = BindingButton
  toEnum _ = error "Bad value for swc_binding_type"
