{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
import           Data.Global
import           Foreign.C.Types
import           Foreign.Storable
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Unsafe as CU

{- Includes -}
C.include "<math.h>"
C.include "<stdio.h>"

C.include "<stdlib.h>"
C.include "<swc.h>"
C.include "<unistd.h>"
C.include "<wayland-server.h>"
C.include "<xkbcommon/xkbcommon.h>"

{- Types -}
data SwcScreen
data SwcWindow

data Screen = Screen {              --struct screen {
  swc        :: SwcScreen,          --        struct swc_screen *swc;
  windows    :: [Window],           --        struct wl_list windows;
  numWindows :: Integer,            --        unsigned num_windows;
}                                   --};

instance Storable Screen where
  sizeOf ~(Screen s w n)    = sizeOf s + sizeOf w + sizeOf n
  alignment ~(Screen s _ _) = alignment s
  peek p                    = do
    s <- peek (castPtr ptr)
    w <- peekByteOff (castPtr ptr) (sizeOf s)
    n <- peekByteOff (castPtr ptr) (sizeOf s + sizeOf d)
    return $ Screen s w n
  poke p (Screen s w n)     = do
    poke (castPtr ptr) s
    pokeByteOff (castPtr ptr) (sizeOf s) w
    pokeByteOff (castPtr ptr) (sizeOf s + sizeOf d) n

data Window = Window {              --struct window {
  swc    :: SwcWindow,              --        struct swc_window *swc;
  screen :: Screen,                 --        struct screen *screen;
  link   :: [Window],               --        struct wl_list link;
}                                   --};

instance Storable Window where
  sizeOf ~(Window s c l)    = sizeOf s + sizeOf c + sizeOf l
  alignment ~(Window s _ _) = alignment s
  peek p                    = do
    s <- peek (castPtr ptr)
    c <- peekByteOff (castPtr ptr) (sizeOf s)
    l <- peekByteOff (castPtr ptr) (sizeOf s + sizeOf c)
    return $ Window s c l
  poke p (Window s c l)     = do
    poke (castPtr ptr) s
    pokeByteOff (castPtr ptr) (sizeOf s) c
    pokeByteOff (castPtr ptr) (sizeOf s + sizeOf c) l

{- Context -}
C.context $ C.baseCtx <> C.funCtx <> C.vecCtx <> mempty {
  C.ctxTypesTable = Map.fromList [
      (C.TypeName "Screen", [t| Screen |]) ,
      (C.TypeName "Window", [t| Window |]) ]
}

{- Constants -}
WL_KEYBOARD_KEY_STATE_PRESSED = (#const WL_KEYBOARD_KEY_STATE_PRESSED) :: CUInt

terminalCommand = [ "st-wl" ]       --static const char *terminal_command[] = { "st-wl", NULL };
dmenuCommand = [ "dmenu_run-wl" ]   --static const char *dmenu_command[] = { "dmenu_run-wl", NULL };
borderWidth = 1                     --static const uint32_t border_width = 1;
borderColourActive = 0xff333388     --static const uint32_t border_color_active = 0xff333388;
borderColourNormal = 0xff888888     --static const uint32_t border_color_normal = 0xff888888;

display = MVar Display
display = declareEmptyMVar "display"

{- Implementation -}
new_screen :: SwcScreen -> IO ()
new_screen swc = do
  windows <-
  [C.block| {
      wl_list_init(&$(screen)->windows);
  } |]
  let screen = Screen swc 0 windows
  [C.block| {
      wl_list_init(&screen->windows);
      swc_screen_set_handler($(swc), &screen_handler, screen);
      active_screen = screen;
  } |]

new_window :: SwcWindow -> IO ()
new_window swc = do
  window <- [C.exp| struct window *window {
      malloc(sizeof(*window))
  } |]
  windowValid <- [C.exp| bool {
      !window
  } |]
  [C.block| {
        struct window *window;

        window = malloc(sizeof(*window));

        if (!window)
                return;

        window->swc = swc;
        window->screen = NULL;
        swc_window_set_handler(swc, &window_handler, window);
        swc_window_set_tiled(swc);
        screen_add_window(active_screen, window);
        focus(window);
  } |]

manager :: IO Ptr
manager cb_new_screen cb_new_window = do
  [C.exp| struct swc_manager {
      { &$fun:(void (*new_screen)(struct swc_screen *)), &$fun:(void (*new_window)(struct swc_window *)) }
  } |]

spawn :: Ptr -> Word32 -> Word32 -> CUInt -> IO ()
spawn ptr_data time value WL_KEYBOARD_KEY_STATE_PRESSED = do
  [C.block| {
      char *const *command = $(void* data);

      if (fork() == 0) {
          execvp(command[0], command);
          exit(EXIT_FAILURE);
      }
  } |]
spawn ptr_data time value state = return ()

quit :: Ptr -> Word32 -> Word32 -> CUInt -> IO ()
quit ptr_data time value WL_KEYBOARD_KEY_STATE_PRESSED = do
  display_val <- readMVar display
  [C.block| {
      wl_display_terminate($(display));
  } |]
quit ptr_data time value state = return ()

main :: IO ()
main = do
  print =<< test_cexp 3 4
  print =<< test_cexp_unsafe 3 4
  test_voidExp
  [C.block| void {
      const char *socket;

      display = wl_display_create();
      if (!display)
              return EXIT_FAILURE;

      socket = wl_display_add_socket_auto(display);
      if (!socket)
              return EXIT_FAILURE;
      setenv("WAYLAND_DISPLAY", socket, 1);

      if (!swc_initialize(display, NULL, &manager))
              return EXIT_FAILURE;

      swc_add_binding(SWC_BINDING_KEY, SWC_MOD_LOGO, XKB_KEY_Return,
                      &spawn, terminal_command);
      swc_add_binding(SWC_BINDING_KEY, SWC_MOD_LOGO, XKB_KEY_r,
                      &spawn, dmenu_command);
      swc_add_binding(SWC_BINDING_KEY, SWC_MOD_LOGO, XKB_KEY_q,
                      &quit, NULL);

      event_loop = wl_display_get_event_loop(display);
      wl_display_run(display);
      wl_display_destroy(display);

      return EXIT_SUCCESS;
  } |]

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
                        --num_rows;

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
        ++screen->num_windows;
        swc_window_show(window->swc);
        arrange(screen);
}

static void
screen_remove_window(struct screen *screen, struct window *window)
{
        window->screen = NULL;
        wl_list_remove(&window->link);
        --screen->num_windows;
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
