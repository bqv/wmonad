{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
module WMonad (
  run
) where

import           Control.Comonad.Store
import           Control.Comonad.Store.Zipper
import           Control.Comonad.Store.Zipper.Circular
import           Data.ByteString
import           Data.Functor
import           Data.IntMap
import           Data.IORef
import           Data.Lens
import qualified Data.Map as Map
import           Data.Word
import           Foreign.C.Types
import           Foreign.ForeignPtr
import           Foreign.Marshal.Utils
import           Foreign.Ptr
import           Foreign.StablePtr
import           Foreign.Storable
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Inline.Unsafe as CU
import qualified Language.C.Types as C
import           WMonad.Internal

C.context wmCtx

{- Includes -}
C.include "<math.h>"
C.include "<stdio.h>"

C.include "<stdlib.h>"
C.include "<swc.h>"
C.include "<unistd.h>"
C.include "<wayland-server.h>"
C.include "<xkbcommon/xkbcommon.h>"

{- Types -}
newtype WM a = State Core a

data Core = Core (Ptr WlEventLoop) Display
--  static struct wl_event_loop *event_loop;

data Display = Display (Ptr WlDisplay) (Zipper IntMap (Maybe Screen))
--  static void *active_screen;
--  static struct wl_display *display;

data Screen = Screen (Ptr SwcScreen) (Zipper IntMap (Maybe Window))
--  static void *focused_window;
--void {
--  struct swc_screen *swc;
--  struct wl_list windows;
--  unsigned num_windows;
--};

data Window = Window (Ptr SwcWindow)
--void {
--  struct swc_window *swc;
--  void *screen;
--  struct wl_list link;
--};

{- Implementation -}
newScreen :: Ptr SwcScreenHandler -> NewScreenCallback
newScreen sh = \p_swc -> do
  screen <- newMVar $ Screen p_swc mempty
  p_screen <- castStablePtrToPtr <$> newStablePtr screen
  [C.block| void {
    swc_screen_set_handler($(struct swc_screen *p_swc), $(struct swc_screen_handler *sh), $(void *p_screen));
    active_screen = $(void *p_screen);
  } |]

newWindow :: Ptr SwcWindowHandler -> NewWindowCallback
newWindow wh = \p_swc -> do
  window <- newMVar $ Window p_swc
  p_window <- castStablePtrToPtr <$> newStablePtr window
  [C.block| void {
    swc_window_set_handler($(struct swc_window *p_swc), $(struct swc_window_handler *wh), $(void *p_window));
    swc_window_set_tiled($(struct swc_window *p_swc));
    screen_add_window(active_screen, $(void *p_window));
    focus($(void *p_window));
  } |]

manager :: NewScreenCallback -> NewWindowCallback -> IO SwcManager
manager ns nw = SwcManager <$> $(C.mkFunPtr [t| NewScreenCallback |]) ns <*> $(C.mkFunPtr [t| NewWindowCallback |]) nw

usableGeometryChanged :: SwcDataCallback
usableGeometryChanged = \p_data -> [C.block| void {
    void *screen = $(void *p_data);

    /* If the usable geometry of the screen changes, for example when a panel is
     * docked to the edge of the screen, we need to rearrange the windows to
     * ensure they are all within the new usable geometry. */
    arrange(screen);
} |]

screenEntered :: SwcDataCallback
screenEntered = \p_data -> [C.block| void {
    void *screen = $(void *p_data);

    active_screen = screen;
} |]

screenHandler :: SwcDataCallback -> SwcDataCallback -> IO SwcScreenHandler
screenHandler ugc se = SwcScreenHandler <$> $(C.mkFunPtr [t| SwcDataCallback |]) ugc <*> $(C.mkFunPtr [t| SwcDataCallback |]) se

windowDestroy :: SwcDataCallback
windowDestroy = \p_data -> [C.block| void {
    void *window = $(void *p_data), *next_focus;

    if (focused_window == window) {
      /* Try to find a new focus nearby the old one. */
      next_focus = wl_container_of(window->link.next, window, link);

      if (&next_focus->link == &window->screen->windows) {
        next_focus = wl_container_of(window->link.prev, window, link);

        if (&next_focus->link == &window->screen->windows)
          next_focus = NULL;
      }

      focus(next_focus);
    }

    screen_remove_window(window->screen, window);
    free(window);
} |]

windowEntered :: SwcDataCallback
windowEntered = \p_data -> [C.block| void {
    void *window = $(void *p_data);

    focus(window);
} |]

windowHandler :: SwcDataCallback -> SwcDataCallback -> IO SwcWindowHandler
windowHandler wd we = SwcWindowHandler <$> $(C.mkFunPtr [t| SwcDataCallback |]) wd <*> $(C.mkFunPtr [t| SwcDataCallback |]) we

arrange :: Ptr Screen -> IO ()
arrange p_screen = [C.block| void {
    /* This is a basic grid arrange function that tries to give each window an
     * equal space. */
    void *window = NULL;
    unsigned num_columns, num_rows, column_index, row_index;
    struct swc_rectangle geometry;
    struct swc_rectangle *screen_geometry = &$(void *p_screen)->swc->usable_geometry

    if ($(void *p_screen)->num_windows == 0)
      return;

    num_columns = ceil(sqrt($(void *p_screen)->num_windows));
    num_rows = $(void *p_screen)->num_windows / num_columns + 1;
    window = wl_container_of($(void *p_screen)->windows.next, window, link);

    for (column_index = 0; &window->link != &$(void *p_screen)->windows; ++column_index) {
      geometry.x = screen_geometry->x + border_width + screen_geometry->width * column_index / num_columns;
      geometry.width = screen_geometry->width / num_columns - 2 * border_width;

      if (column_index == $(void *p_screen)->num_windows % num_columns)
        num_rows--;

      for (row_index = 0; row_index < num_rows; ++row_index) {
        geometry.y = screen_geometry->y + border_width + screen_geometry->height * row_index / num_rows;
        geometry.height = screen_geometry->height / num_rows - 2 * border_width;

        swc_window_set_geometry(window->swc, &geometry);
        window = wl_container_of(window->link.next, window, link);
      }
    }
} |]

addWindow :: Ptr Screen -> Ptr Window -> IO ()
addWindow p_screen p_window = [C.block| void {
     $(void *p_window)->screen = $(void *p_screen);
     wl_list_insert(&($(void *p_screen)->windows), &($(void *p_window)->link));
     $(void *p_screen)->num_windows++;
     swc_window_show($(void *p_window)->swc);
     arrange($(void *p_screen));
} |]

removeWindow :: Ptr Screen -> Ptr Window -> IO ()
removeWindow p_screen p_window = [C.block| void {
     $(void *p_window)->screen = NULL;
     wl_list_remove(&($(void *p_window)->link));
     $(void *p_screen)->num_windows--;
     swc_window_hide($(void *p_window)->swc);
     arrange($(void *p_screen));
} |]

focus :: Ptr Window -> IO ()
focus p_window = [C.block| void {
     if (focused_window) {
       swc_window_set_border(focused_window->swc, border_color_normal, border_width);
     }

     if ($(void *p_window)) {
       swc_window_set_border($(void *p_window)->swc, border_color_active, border_width);
       swc_window_focus($(void *p_window)->swc);
     } else
       swc_window_focus(NULL);

     focused_window = $(void *p_window);
} |]

spawn :: SwcBindingCallback
spawn p_data time value state
  | (toEnum.fromEnum) state == WlKeyboardKeyStatePressed = do
    [C.block| void {
      char *const *command = $(void *p_data);

      if (fork() == 0) {
        execvp(command[0], command);
        exit(EXIT_FAILURE);
      }
    } |]
  | otherwise = return ()

quit :: Ptr WlDisplay -> SwcBindingCallback
quit display p_data time value state
  | (toEnum.fromEnum) state == WlKeyboardKeyStatePressed = do
    [C.block| void {
      wl_display_terminate($(struct wl_display *display));
    } |]
  | otherwise = return ()

mkStablePtr :: a -> IO (Ptr a)
mkStablePtr val = newStablePtr val <&> (castPtr . castStablePtrToPtr)

run :: IO ()
run = do
  hScreen <- screenHandler usableGeometryChanged screenEntered >>= mkStablePtr
  let fnNewScreen = newScreen hScreen
  hWindow <- windowHandler windowDestroy windowEntered >>= mkStablePtr
  let fnNewWindow = newWindow hWindow
  p_manager <- manager fnNewScreen fnNewWindow >>= mkStablePtr

  display <- newDisplay
  p_display <- readIORef display
  let cbQuit = quit p_display

  [C.block| void {
    const char *socket = wl_display_add_socket_auto($(struct wl_display *p_display));
    if (!socket)
      return EXIT_FAILURE;
    setenv("WAYLAND_DISPLAY", socket, 1);
  } |]

  [C.block| void {
    if (!swc_initialize($(struct wl_display *p_display), NULL, $(struct swc_manager *p_manager)))
      return EXIT_FAILURE;

    swc_add_binding(SWC_BINDING_KEY, SWC_MOD_LOGO, XKB_KEY_Return,
                    $fun:(void (*spawn)(void *, uint32_t, uint32_t, uint32_t)), terminal_command);
    swc_add_binding(SWC_BINDING_KEY, SWC_MOD_LOGO, XKB_KEY_r,
                    $fun:(void (*spawn)(void *, uint32_t, uint32_t, uint32_t)), dmenu_command);
    swc_add_binding(SWC_BINDING_KEY, SWC_MOD_LOGO, XKB_KEY_q,
                    $fun:(void (*cbQuit)(void *, uint32_t, uint32_t, uint32_t)), NULL);

    event_loop = wl_display_get_event_loop($(struct wl_display *p_display));
    wl_display_run($(struct wl_display *p_display));
    wl_display_destroy($(struct wl_display *p_display));
  } |]
  where
    ensurePtr :: String -> Ptr a -> IO (Ptr a)
    ensurePtr errMsg p_val = do
      let ptr = castPtr p_val
      notNull <- [C.exp| bool { !$(void *ptr) } |]
      if toBool notNull then return p_val else fail errMsg
    newDisplay :: IO (IORef (Ptr WlDisplay))
    newDisplay = [C.exp| struct wl_display * {
      wl_display_create();
    } |] >>= ensurePtr "error: wl_display_create" >>= newIORef
