{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Graphics.Wayland.SWC.Types where

import           Prelude
import           Control.Monad
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.Enum.Util
import qualified Data.Map as Map
import           Data.Time hiding (parseTime)
import           Data.Time.Clock.POSIX
import           Data.Word
import           Foreign.Callable
import           Foreign.Callback
import           Foreign.C.Types
import           Foreign.Ptr
import           Foreign.Storable
import qualified Language.Haskell.TH as TH
import qualified Graphics.Wayland.Server as WL
import qualified Graphics.Wayland.Server.Events as WL
import           System.InputDevice
import           Text.XkbCommon
import           Text.XkbCommon.InternalTypes

keysymToC :: Integral a => Keysym -> a
keysymToC = fromIntegral . unCKeysym . fromKeysym

keysymFromC :: Integral a => a -> Keysym
keysymFromC = toKeysym . CKeysym . fromIntegral

#include <swc.h>

{#context prefix = "swc" #}

{#pointer *libinput_device as InputDevice nocode #}

type NewScreenCallback = Ptr Screen -> IO ()
type NewScreenFunc = Ptr Screen -> IO ()
foreign import ccall "wrapper"
  _wrapNewScreenFunc :: NewScreenFunc -> IO (FunPtr NewScreenFunc)
instance Callback NewScreenCallback NewScreenFunc where
  wrapF f = _wrapNewScreenFunc $ f

type NewWindowCallback = Ptr Window -> IO ()
type NewWindowFunc = Ptr Window -> IO ()
foreign import ccall "wrapper"
  _wrapNewWindowFunc :: NewWindowFunc -> IO (FunPtr NewWindowFunc)
instance Callback NewWindowCallback NewWindowFunc where
  wrapF f = _wrapNewWindowFunc $ f

type NewDeviceCallback = InputDevice -> IO ()
type NewDeviceFunc = InputDevice -> IO ()
foreign import ccall "wrapper"
  _wrapNewDeviceFunc :: NewDeviceFunc -> IO (FunPtr NewDeviceFunc)
instance Callback NewDeviceCallback NewDeviceFunc where
  wrapF f = _wrapNewDeviceFunc $ f

type SessionCallback = IO ()
type SessionFunc = IO ()
foreign import ccall "wrapper"
  _wrapSessionFunc :: SessionFunc -> IO (FunPtr SessionFunc)
instance Callback SessionCallback SessionFunc where
  wrapF = _wrapSessionFunc

type DataCallback a = Ptr a -> IO ()
type DataFunc a = Ptr () -> IO ()
foreign import ccall "wrapper"
  _wrapDataFunc :: DataFunc a -> IO (FunPtr (DataFunc a))
instance Callback (DataCallback a) (DataFunc a) where
  wrapF f = _wrapDataFunc $ f . castPtr

type BindingCallback = UTCTime -> Keysym -> WL.KeyboardKeyState -> IO ()
type BindingFunc = Ptr () -> CUInt -> CUInt -> CUInt -> IO ()
foreign import ccall "wrapper"
  _wrapBindingFunc :: BindingFunc -> IO (FunPtr BindingFunc)
instance Callback BindingCallback BindingFunc where
  wrapF f = _wrapBindingFunc $ \_ t v s -> mapEnum
    <$> f (posixSecondsToUTCTime . realToFrac $ t) (keysymFromC v) (mapEnum s)
instance {-# OVERLAPPING #-} Arg BindingCallback where
  arg _ = ArgStr "callback"

{#pointer *swc_rectangle -> Rectangle nocode #}
data Rectangle = Rectangle {
  posX :: Int,
  posY :: Int,
  sizeW :: Word,
  sizeH :: Word
} deriving (Eq, Show)

instance Storable Rectangle where
  sizeOf _                        = {#sizeof swc_rectangle #}
  alignment (Rectangle _ _ _ _)   = {#alignof swc_rectangle #}
  peek p                          = do
    x <- fromIntegral <$> {#get struct swc_rectangle->x #} p
    y <- fromIntegral <$> {#get struct swc_rectangle->y #} p
    w <- fromIntegral <$> {#get struct swc_rectangle->width #} p
    h <- fromIntegral <$> {#get struct swc_rectangle->height #} p
    return $ Rectangle x y w h
  poke p (Rectangle x y w h)      = do
    {#set struct swc_rectangle->x #} p . fromIntegral $ x
    {#set struct swc_rectangle->y #} p . fromIntegral $ y
    {#set struct swc_rectangle->width #} p . fromIntegral $ w
    {#set struct swc_rectangle->height #} p . fromIntegral $ h

{#pointer *swc_screen_handler -> ScreenHandler nocode #}
data ScreenHandler = ScreenHandler {
  fnScreenDestroy :: DataCallback Screen,
  fnGeometryChanged :: DataCallback Screen,
  fnUsableGeometryChanged :: DataCallback Screen,
  fnScreenEntered :: DataCallback Screen
} deriving ()

instance Storable ScreenHandler where
  sizeOf _                            = {#sizeof swc_screen_handler #}
  alignment (ScreenHandler _ _ _ _)   = {#alignof swc_screen_handler #}
  peek p                              = do undefined
  --sd <- {#get struct swc_screen_handler->destroy #} p
  --gc <- {#get struct swc_screen_handler->geometry_changed #} p
  --ugc <- {#get struct swc_screen_handler->usable_geometry_changed #} p
  --se <- {#get struct swc_screen_handler->entered #} p
  --return $ ScreenHandler sd gc ugc se
  poke p (ScreenHandler sd gc ugc se) = do
    wrapF sd >>= {#set struct swc_screen_handler->destroy #} p
    wrapF gc >>= {#set struct swc_screen_handler->geometry_changed #} p
    wrapF ugc >>= {#set struct swc_screen_handler->usable_geometry_changed #} p
    wrapF se >>= {#set struct swc_screen_handler->entered #} p

{#pointer *swc_screen as Screen_ -> Screen #}
data Screen = Screen {
  screenGeometry :: Ptr Rectangle,
  screenUsableGeometry :: Ptr Rectangle
} deriving (Show)

instance Storable Screen where
  sizeOf _               = {#sizeof swc_screen #}
  alignment (Screen _ _) = {#alignof swc_screen #}
  peek p                 = do
    g <- castPtr <$> {#get struct swc_screen->geometry #} p
    ug <- castPtr <$> {#get struct swc_screen->usable_geometry #} p
    return $ Screen g ug
  poke p (Screen g ug)   = do
    {#set struct swc_screen->geometry #} p . castPtr $ g
    {#set struct swc_screen->usable_geometry #} p . castPtr $ ug

{#pointer *swc_window_handler -> WindowHandler nocode #}
data WindowHandler = WindowHandler {
  fnWindowDestroy :: DataCallback Window,
  fnTitleChanged :: DataCallback Window,
  fnAppIdChanged :: DataCallback Window,
  fnParentChanged :: DataCallback Window,
  fnWindowEntered :: DataCallback Window,
  fnWindowMove :: DataCallback Window,
  fnWindowResize :: DataCallback Window
} deriving ()

instance Storable WindowHandler where
  sizeOf _                                     = {#sizeof swc_window_handler #}
  alignment (WindowHandler _ _ _ _ _ _ _)      = {#alignof swc_window_handler #}
  peek p                                       = do undefined
  --wd <- {#get struct swc_window_handler->destroy #} p
  --tc <- {#get struct swc_window_handler->title_changed #} p
  --aic <- {#get struct swc_window_handler->app_id_changed #} p
  --pc <- {#get struct swc_window_handler->parent_changed #} p
  --we <- {#get struct swc_window_handler->entered #} p
  --wm <- {#get struct swc_window_handler->move #} p
  --wr <- {#get struct swc_window_handler->resize #} p
  --return $ WindowHandler wd tc aic pc we wm wr
  poke p (WindowHandler wd tc aic pc we wm wr) = do
    wrapF wd >>= {#set struct swc_window_handler->destroy #} p
    wrapF tc >>= {#set struct swc_window_handler->title_changed #} p
    wrapF aic >>= {#set struct swc_window_handler->app_id_changed #} p
    wrapF pc >>= {#set struct swc_window_handler->parent_changed #} p
    wrapF we >>= {#set struct swc_window_handler->entered #} p
    wrapF wm >>= {#set struct swc_window_handler->move #} p
    wrapF wr >>= {#set struct swc_window_handler->resize #} p

{#pointer *swc_window as Window_ -> Window #}
data Window = Window {
  windowTitle :: ByteString,
  windowAppId :: ByteString,
  windowParent :: Ptr Window
} deriving (Show)

instance Storable Window where
  sizeOf _                 = {#sizeof swc_window #}
  alignment (Window _ _ _) = {#alignof swc_window #}
  peek p                   = do
    t <- BS.packCString =<< {#get struct swc_window->title #} p
    ai <- BS.packCString =<< {#get struct swc_window->app_id #} p
    par <- castPtr <$> {#get struct swc_window->parent #} p
    return $ Window t ai par
  poke p (Window t ai par) = do
    BS.useAsCString t $ {#set struct swc_window->title #} p
    BS.useAsCString ai $ {#set struct swc_window->app_id #} p
    {#set struct swc_window.parent #} p par

{#pointer *swc_manager -> Manager nocode #}
data Manager = Manager {
  fnNewScreen :: NewScreenCallback,
  fnNewWindow :: NewWindowCallback,
  fnNewDevice :: NewDeviceCallback,
  fnActivate :: SessionCallback,
  fnDeactivate :: SessionCallback
} deriving ()

instance Storable Manager where
  sizeOf _                      = {#sizeof swc_manager #}
  alignment (Manager _ _ _ _ _) = {#alignof swc_manager #}
  peek p                        = do undefined
  --ns <- {#get struct swc_manager->new_screen #} p
  --nw <- {#get struct swc_manager->new_window #} p
  --nd <- {#get struct swc_manager->new_device #} p
  --a <- {#get struct swc_manager->activate #} p
  --d <- {#get struct swc_manager->deactivate #} p
  --return $ Manager ns nw nd a d
  poke p (Manager ns nw nd a d) = do
    wrapF ns >>= {#set struct swc_manager->new_screen #} p
    wrapF nw >>= {#set struct swc_manager->new_window #} p
    wrapF nd >>= {#set struct swc_manager->new_device #} p
    wrapF a >>= {#set struct swc_manager->activate #} p
    wrapF d >>= {#set struct swc_manager->deactivate #} p

{#enum define WindowEdge {
    SWC_WINDOW_EDGE_AUTO as EdgeAuto,
    SWC_WINDOW_EDGE_TOP as EdgeTop,
    SWC_WINDOW_EDGE_BOTTOM as EdgeBottom,
    SWC_WINDOW_EDGE_LEFT as EdgeLeft,
    SWC_WINDOW_EDGE_RIGHT as EdgeRight}
  deriving (Eq, Show) #}

{#enum define Modifier {
    SWC_MOD_CTRL as ModifierCtrl,
    SWC_MOD_ALT as ModifierAlt,
    SWC_MOD_LOGO as ModifierLogo,
    SWC_MOD_SHIFT as ModifierShift}
  deriving (Show, Ord) #}

instance Eq Modifier where
  (==) a b = fromEnum a == fromEnum b

instance Semigroup Modifier where
  (<>) a b = toEnum $ fromEnum a + fromEnum b

{#enum swc_binding_type as BindingType {
    SWC_BINDING_KEY as BindingKey,
    SWC_BINDING_BUTTON as BindingButton}
  deriving (Eq, Show, Ord) #}

addBinding = traceC4 "swc_add_binding" addBinding'
addBinding' :: BindingType -> Modifier -> Keysym -> BindingCallback -> IO Int
addBinding' t m k c = join $ addBinding'' t m k <$> wrapF c <*> pure nullPtr
{#fun swc_add_binding as addBinding''
 { mapEnum `BindingType', mapEnum `Modifier', keysymToC `Keysym', id `FunPtr BindingFunc', `Ptr ()' } -> `Int' #}
