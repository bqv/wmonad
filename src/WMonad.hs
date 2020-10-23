{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module WMonad (
  run
) where

import           Control.Concurrent.MVar
import           Data.ByteString
import           Data.Functor
import           Data.IORef
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
data Screen = Screen (Ptr SwcScreen) [Window] Int
--struct screen {
--  struct swc_screen *swc;
--  struct wl_list windows;
--  unsigned num_windows;
--};

data Window = Window (Ptr SwcWindow) (Ptr Screen)
--struct window {
--  struct swc_window *swc;
--  struct screen *screen;
--  struct wl_list link;
--};

{- Implementation -}
newScreen :: NewScreenCallback
newScreen = \p_swc -> do
  screen <- newMVar $ Screen p_swc [] 0
  p_screen <- castStablePtrToPtr <$> newStablePtr screen
  [C.block| void {
    swc_screen_set_handler($(struct swc_screen *p_swc), &screen_handler, $(void *p_screen));
    active_screen = $(void *p_screen);
  } |]

newWindow :: NewWindowCallback
newWindow = \p_swc -> do
  window <- newMVar $ Window p_swc nullPtr
  p_window <- castStablePtrToPtr <$> newStablePtr window
  [C.block| void {
    swc_window_set_handler($(struct swc_window *p_swc), &window_handler, $(void *p_window));
    swc_window_set_tiled($(struct swc_window *p_swc));
    screen_add_window(active_screen, $(void *p_window));
    focus($(void *p_window));
  } |]

manager :: NewScreenCallback -> NewWindowCallback -> IO SwcManager
manager ns nw = SwcManager <$> $(C.mkFunPtr [t| NewScreenCallback |]) ns <*> $(C.mkFunPtr [t| NewWindowCallback |]) nw

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

run :: IO ()
run = do
  p_manager <- manager newScreen newWindow >>= newStablePtr <&> (castPtr . castStablePtrToPtr)
  display <- newDisplay
  p_display <- readIORef display
  let cbQuit = quit p_display
  [C.block| void {
    const char *socket = wl_display_add_socket_auto($(struct wl_display *p_display));
    if (!socket)
      return EXIT_FAILURE;
    setenv("WAYLAND_DISPLAY", socket, 1);

    if (!swc_initialize($(struct wl_display *p_display), NULL, $(struct swc_manager *p_manager)))
      return EXIT_FAILURE;

    swc_add_binding(SWC_BINDING_KEY, SWC_MOD_LOGO, XKB_KEY_Return,
                    &spawn, terminal_command);
    swc_add_binding(SWC_BINDING_KEY, SWC_MOD_LOGO, XKB_KEY_r,
                    &spawn, dmenu_command);
    swc_add_binding(SWC_BINDING_KEY, SWC_MOD_LOGO, XKB_KEY_q,
                    &$fun:(void (*cbQuit)(void *, uint32_t, uint32_t, uint32_t)), NULL);

    event_loop = wl_display_get_event_loop($(struct wl_display *p_display));
    wl_display_run($(struct wl_display *p_display));
    wl_display_destroy($(struct wl_display *p_display));

    return EXIT_SUCCESS;
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

rest = [C.block| void {
        static struct screen *active_screen;
        static struct window *focused_window;
        static struct wl_display *display;
        static struct wl_event_loop *event_loop;

        /* This is a basic grid arrange function that tries to give each window an
         * equal space. */
        static void
        arrange(struct screen *screen)
        {
                struct window *window = NULL;
                unsigned num_columns, num_rows, column_index, row_index;
                struct swc_rectangle geometry;
                struct swc_rectangle *screen_geometry = &screen->swc->usable_geometry;

                if (screen->num_windows == 0)
                        return;

                num_columns = ceil(sqrt(screen->num_windows));
                num_rows = screen->num_windows / num_columns + 1;
                window = wl_container_of(screen->windows.next, window, link);

                for (column_index = 0; &window->link != &screen->windows; ++column_index) {
                        geometry.x = screen_geometry->x + border_width
                                     + screen_geometry->width * column_index / num_columns;
                        geometry.width = screen_geometry->width / num_columns
                                         - 2 * border_width;

                        if (column_index == screen->num_windows % num_columns)
                                num_rows--;

                        for (row_index = 0; row_index < num_rows; ++row_index) {
                                geometry.y = screen_geometry->y + border_width
                                             + screen_geometry->height * row_index / num_rows;
                                geometry.height = screen_geometry->height / num_rows
                                                  - 2 * border_width;

                                swc_window_set_geometry(window->swc, &geometry);
                                window = wl_container_of(window->link.next, window, link);
                        }
                }
        }

        static void
        screen_add_window(struct screen *screen, struct window *window)
        {
                window->screen = screen;
                wl_list_insert(&screen->windows, &window->link);
                screen->num_windows++;
                swc_window_show(window->swc);
                arrange(screen);
        }

        static void
        screen_remove_window(struct screen *screen, struct window *window)
        {
                window->screen = NULL;
                wl_list_remove(&window->link);
                screen->num_windows--;
                swc_window_hide(window->swc);
                arrange(screen);
        }

        static void
        focus(struct window *window)
        {
                if (focused_window) {
                        swc_window_set_border(focused_window->swc,
                                              border_color_normal, border_width);
                }

                if (window) {
                        swc_window_set_border(window->swc, border_color_active, border_width);
                        swc_window_focus(window->swc);
                } else
                        swc_window_focus(NULL);

                focused_window = window;
        }
} |]

c_screen_handler = [C.block| void {
        static void
        screen_usable_geometry_changed(void *data)
        {
                struct screen *screen = data;

                /* If the usable geometry of the screen changes, for example when a panel is
                 * docked to the edge of the screen, we need to rearrange the windows to
                 * ensure they are all within the new usable geometry. */
                arrange(screen);
        }

        static void
        screen_entered(void *data)
        {
                struct screen *screen = data;

                active_screen = screen;
        }

        static const struct swc_screen_handler screen_handler = {
                .usable_geometry_changed = &screen_usable_geometry_changed,
                .entered = &screen_entered,
        };
} |]

c_window_handler = [C.block| void {
        static void
        window_destroy(void *data)
        {
                struct window *window = data, *next_focus;

                if (focused_window == window) {
                        /* Try to find a new focus nearby the old one. */
                        next_focus = wl_container_of(window->link.next, window, link);

                        if (&next_focus->link == &window->screen->windows) {
                                next_focus = wl_container_of(window->link.prev,
                                                             window, link);

                                if (&next_focus->link == &window->screen->windows)
                                        next_focus = NULL;
                        }

                        focus(next_focus);
                }

                screen_remove_window(window->screen, window);
                free(window);
        }

        static void
        window_entered(void *data)
        {
                struct window *window = data;

                focus(window);
        }

        static const struct swc_window_handler window_handler = {
                .destroy = &window_destroy,
                .entered = &window_entered,
        };
} |]
