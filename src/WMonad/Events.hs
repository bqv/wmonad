module WMonad.Events (
  module WMonad.Events.InputEvent,
  module WMonad.Events
) where

import           Protolude
import           Colog.Polysemy.Formatting
import           Control.Comonad.Store.Zipper
import           Control.Monad.Trans.State
import qualified Data.Map as M
import           Data.Time.Clock
import qualified Graphics.Wayland.Server as WL
import qualified Polysemy as P
import qualified Polysemy.Writer as P
import qualified Polysemy.Reader as P
import qualified Polysemy.Input as P
import qualified Polysemy.Embed as P
import qualified Polysemy.Error as P
import qualified Polysemy.State as P
import qualified Polysemy.View as P
import qualified Text.XkbCommon as XKB
import           WMonad.Events.InputEvent
import qualified WMonad.SWC as SWC
import           WMonad.Types

{-
newScreen :: (MonadState Core m) => Ptr SWC.ScreenHandler -> m SWC.NewScreenCallback
newScreen sh = \p_swc -> do
  let screen = Screen p_swc mempty
  p_screen <- castStablePtrToPtr <$> newStablePtr screen
  [C.block| void {
    swc_screen_set_handler($(struct swc_screen *p_swc), $(struct swc_screen_handler *sh), $(void *p_screen));
    active_screen = $(void *p_screen);
  } |]

usableGeometryChanged :: SWC.DataCallback
usableGeometryChanged = \p_data -> [C.block| void {
    void *screen = $(void *p_data);

    /* If the usable geometry of the screen changes, for example when a panel is
     * docked to the edge of the screen, we need to rearrange the windows to
     * ensure they are all within the new usable geometry. */
    arrange(screen);
} |]

screenEntered :: SWC.DataCallback
screenEntered = \p_data -> [C.block| void {
    void *screen = $(void *p_data);

    active_screen = screen;
} |]

windowDestroy :: SWC.DataCallback
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

windowEntered :: SWC.DataCallback
windowEntered = \p_data -> [C.block| void {
    void *window = $(void *p_data);

    focus(window);
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
-}
