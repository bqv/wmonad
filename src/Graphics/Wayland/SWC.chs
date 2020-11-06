module Graphics.Wayland.SWC where

import           Prelude
import           Control.Monad.Trans.State
import           Data.Enum.Util
import qualified Data.Map as Map
import           Data.Ratio ((%))
import           Data.Time.Clock (DiffTime)
import           Data.Fixed (Fixed(..), HasResolution(..), Milli(..))
import qualified Data.Text as T
import           Data.Typeable
import           Data.Functor
import           Foreign
import           Foreign.Callable
import           Foreign.C.Types
import           Foreign.C.String
import qualified Graphics.Wayland.Server as WL
import           Graphics.Wayland.SWC.Colour
import           Graphics.Wayland.SWC.Types

#include <swc.h>
#include <wayland-server.h>

{#context prefix="wl" #}

screenSetHandler = traceC2 "swc_screen_set_handler" screenSetHandler'
screenSetHandler' :: Ptr Screen -> Ptr ScreenHandler -> IO ()
screenSetHandler' s h = screenSetHandler'' s h nullPtr
{#fun swc_screen_set_handler as screenSetHandler''
  { castPtr `Ptr Screen', castPtr `Ptr ScreenHandler', id `Ptr ()' } -> `()' #}

windowSetHandler = traceC2 "swc_window_set_handler" windowSetHandler'
windowSetHandler' :: Ptr Window -> Ptr WindowHandler -> IO ()
windowSetHandler' w h = windowSetHandler'' w h nullPtr
{#fun swc_window_set_handler as windowSetHandler''
  { castPtr `Ptr Window', castPtr `Ptr WindowHandler', id `Ptr ()' } -> `()' #}

windowClose = traceC1 "swc_window_close" windowClose'
windowClose' :: Ptr Window -> IO ()
windowClose' w = windowClose'' w
{#fun swc_window_close as windowClose''
  { castPtr `Ptr Window' } -> `()' #}

windowShow = traceC1 "swc_window_show" windowShow'
windowShow' :: Ptr Window -> IO ()
windowShow' w = windowShow'' w
{#fun swc_window_show as windowShow''
  { castPtr `Ptr Window' } -> `()' #}

windowHide = traceC1 "swc_window_hide" windowHide'
windowHide' :: Ptr Window -> IO ()
windowHide' w = windowHide'' w
{#fun swc_window_hide as windowHide''
  { castPtr `Ptr Window' } -> `()' #}

windowFocus = traceC1 "swc_window_focus" windowFocus'
windowFocus' :: Ptr Window -> IO ()
windowFocus' w = windowFocus'' w
{#fun swc_window_focus as windowFocus''
  { castPtr `Ptr Window' } -> `()' #}

windowSetStacked = traceC1 "swc_window_set_stacked" windowSetStacked'
windowSetStacked' :: Ptr Window -> IO ()
windowSetStacked' w = windowSetStacked'' w
{#fun swc_window_set_stacked as windowSetStacked''
  { castPtr `Ptr Window' } -> `()' #}

windowSetTiled = traceC1 "swc_window_set_tiled" windowSetTiled'
windowSetTiled' :: Ptr Window -> IO ()
windowSetTiled' w = windowSetTiled'' w
{#fun swc_window_set_tiled as windowSetTiled''
  { castPtr `Ptr Window' } -> `()' #}

windowSetFullscreen = traceC2 "swc_window_set_fullscreen" windowSetFullscreen'
windowSetFullscreen' :: Ptr Window -> Ptr Screen -> IO ()
windowSetFullscreen' w s = windowSetFullscreen'' w s
{#fun swc_window_set_fullscreen as windowSetFullscreen''
  { castPtr `Ptr Window', castPtr `Ptr Screen' } -> `()' #}

windowSetPosition = traceC3 "swc_window_set_position" windowSetPosition'
windowSetPosition' :: Ptr Window -> Int -> Int -> IO ()
windowSetPosition' w x y = windowSetPosition'' w x y
{#fun swc_window_set_position as windowSetPosition''
  { castPtr `Ptr Window', fromIntegral `Int', fromIntegral `Int' } -> `()' #}

windowSetSize = traceC3 "swc_window_set_size" windowSetSize'
windowSetSize' :: Ptr Window -> Word -> Word -> IO ()
windowSetSize' w l h = windowSetSize'' w l h
{#fun swc_window_set_size as windowSetSize''
  { castPtr `Ptr Window', fromIntegral `Word', fromIntegral `Word' } -> `()' #}

windowSetGeometry = traceC2 "swc_window_set_geometry" windowSetGeometry'
windowSetGeometry' :: Ptr Window -> Ptr Rectangle -> IO ()
windowSetGeometry' w g = windowSetGeometry'' w g
{#fun swc_window_set_geometry as windowSetGeometry''
  { castPtr `Ptr Window', castPtr `Ptr Rectangle' } -> `()' #}

windowSetBorder = traceC3 "swc_window_set_border" windowSetBorder'
windowSetBorder' :: Ptr Window -> Colour -> Word -> IO ()
windowSetBorder' w c s = windowSetBorder'' w (rgbToC c) s
{#fun swc_window_set_border as windowSetBorder''
  { castPtr `Ptr Window', fromIntegral `Word32', fromIntegral `Word' } -> `()' #}

windowBeginMove = traceC1 "swc_window_begin_move" windowBeginMove'
windowBeginMove' :: Ptr Window -> IO ()
windowBeginMove' w = windowBeginMove'' w
{#fun swc_window_begin_move as windowBeginMove''
  { castPtr `Ptr Window' } -> `()' #}

windowEndMove = traceC1 "swc_window_end_move" windowEndMove'
windowEndMove' :: Ptr Window -> IO ()
windowEndMove' w = windowEndMove'' w
{#fun swc_window_end_move as windowEndMove''
  { castPtr `Ptr Window' } -> `()' #}

windowBeginResize = traceC2 "swc_window_begin_resize" windowBeginResize'
windowBeginResize' :: Ptr Window -> WindowEdge -> IO ()
windowBeginResize' w e = windowBeginResize'' w e
{#fun swc_window_begin_resize as windowBeginResize''
  { castPtr `Ptr Window', mapEnum `WindowEdge' } -> `()' #}

windowEndResize = traceC1 "swc_window_end_resize" windowEndResize'
windowEndResize' :: Ptr Window -> IO ()
windowEndResize' w = windowEndResize'' w
{#fun swc_window_end_resize as windowEndResize''
  { castPtr `Ptr Window' } -> `()' #}

initialize = traceC2 "swc_initialize" initialize'
initialize' :: Ptr WL.Display -> Ptr Manager -> IO (Maybe ())
initialize' d m = initialize'' d nullPtr m >>= \r ->
  pure $ if r then Just () else Nothing
{#fun swc_initialize as initialize''
  { castPtr `Ptr WL.Display', id `Ptr ()', castPtr `Ptr Manager' } -> `Bool' toBool #}

finalize = traceC0 "swc_finalize" finalize'
finalize' :: IO ()
finalize' = {#call unsafe swc_finalize #}
