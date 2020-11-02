{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
module WMonad (
  runCompositor
) where

import           Prelude hiding (log)
import           Colog.Polysemy.Formatting
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Concurrent.STM.TChan
import           Control.Comonad.Store
import           Control.Comonad.Store.Zipper
import           Control.Comonad.Store.Zipper.Circular
import           Control.Exception
import           Control.Monad
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.State
import           Control.Monad.Trans.Writer
import qualified Control.Monad.Morph as MM
import qualified Data.ByteString as BS
import           Data.Either
import           Data.Function
import           Data.Functor
import qualified Data.IntMap as IM
import           Data.IORef
import           Data.String
import qualified Data.Text as T
import           Data.Time.Clock
import           Control.Lens
import qualified Data.Map as M
import           Data.Word
import           Formatting
import           Formatting.Formatters
import           Foreign.C.Types
import           Foreign.Marshal.Utils
import           Foreign.Ptr
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Inline.Unsafe as CU
import qualified Language.C.Types as C
import qualified Polysemy as P
import qualified Polysemy.Writer as P
import qualified Polysemy.Reader as P
import qualified Polysemy.Input as P
import qualified Polysemy.Embed as P
import qualified Polysemy.Final as P
import qualified Polysemy.Error as P
import qualified Polysemy.State as P
import qualified SWC
import qualified SWC.Wayland as WL
import qualified Text.XkbCommon as XKB
import qualified Text.XkbCommon.KeysymList as XKB

{- Types -}
type ZipperMap a = Maybe (Zipper (IM.IntMap) (Maybe a))

data Core = Core { _eventLoop :: Ptr WL.EventLoop, _display :: Display, _ffiThread :: ThreadId }

data Display = Display { _wlDisplay :: Ptr WL.Display, _screens :: ZipperMap Screen }

data Screen = Screen { _swcScreen :: Ptr SWC.Screen, _windows :: ZipperMap Window }

data Window = Window { _swcWindow :: Ptr SWC.Window }

$(makeLenses ''Core)
$(makeLenses ''Display)
$(makeLenses ''Screen)
$(makeLenses ''Window)

{- Implementation -}
{-
newScreen :: (MonadState Core m) => Ptr SWC.ScreenHandler -> m SWC.NewScreenCallback
newScreen sh = \p_swc -> do
  let screen = Screen p_swc mempty
  p_screen <- castStablePtrToPtr <$> newStablePtr screen
  [C.block| void {
    swc_screen_set_handler($(struct swc_screen *p_swc), $(struct swc_screen_handler *sh), $(void *p_screen));
    active_screen = $(void *p_screen);
  } |]

newWindow :: Ptr SWC.WindowHandler -> SWC.NewWindowCallback
newWindow wh = \p_swc -> do
  window <- newMVar $ Window p_swc
  p_window <- castStablePtrToPtr <$> newStablePtr window
  [C.block| void {
    swc_window_set_handler($(struct swc_window *p_swc), $(struct swc_window_handler *wh), $(void *p_window));
    swc_window_set_tiled($(struct swc_window *p_swc));
    screen_add_window(active_screen, $(void *p_window));
    focus($(void *p_window));
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

spawn :: SWC.BindingCallback
spawn p_data time value state
  | (toEnum.fromEnum) state == WL.KeyboardKeyStatePressed = do
    [C.block| void {
      char *const *command = $(void *p_data);

      if (fork() == 0) {
        execvp(command[0], command);
        exit(EXIT_FAILURE);
      }
    } |]
  | otherwise = pure ()
-}

handleInput :: (WithLog r, P.Members '[P.Reader Core, P.Embed IO] r)
            => SWC.Binding -> UTCTime -> Word32 -> WL.KeyboardKeyState -> P.Sem r ()
handleInput (SWC.Binding SWC.BindingKey SWC.ModifierLogo keysym_q) time keyValue keyState
  | keyState == WL.KeyboardKeyStatePressed = do
      askWlDisplay >>= void . spawnFFI . SWC.terminate
handleInput _ _ _ _ = pure ()

--type WM = ExceptT String (ReaderT Bindings (StateT Core IO))

bindings :: (SWC.MonadSwc cb, SWC.MonadWl cb) => Writer (M.Map SWC.Binding (cb ())) ()
bindings = do
--tell ((SWC.Binding SWC.BindingKey SWC.ModifierLogo XKB.keysym_Return), (spawn, Just "st-wl"))
--tell ((SWC.Binding SWC.BindingKey SWC.ModifierLogo XKB.keysym_r), (spawn, Just "dmenu_run-wl"))
--tell ((SWC.Binding SWC.BindingKey SWC.ModifierLogo XKB.keysym_q), (quit, Nothing))
  pure ()

spawnFFI :: (P.Member (P.Embed IO) r) => IO a -> P.Sem r ThreadId
spawnFFI = P.embed . forkOS . void

loopDispatch :: (WithLog r, P.Members '[P.Embed IO, P.Reader Core, P.Input SWC.CallbackEvent] r) => P.Sem r ()
loopDispatch = do
  event <- P.input
  logDebug ("Event: " % shown) (event)
  case event of
    SWC.Catastrophe { SWC.exception } -> do
      logError ("Error: " % string) $ displayException exception
      logDebug "Exiting due to previous error"
      askWlDisplay >>= void . spawnFFI . SWC.terminate
    SWC.Ready _ _ -> undefined
    SWC.InputEvent { SWC.binding, SWC.time, SWC.keyValue, SWC.keyState } -> do
      handleInput binding time keyValue keyState >> loopDispatch
    _ -> loopDispatch -- undefined

askWlDisplay :: (P.Member (P.Reader Core) r) => P.Sem r (Ptr WL.Display)
askWlDisplay = P.asks . view $ display . wlDisplay

getWlDisplay :: (P.Member (P.State Core) r) => P.Sem r (Ptr WL.Display)
getWlDisplay = P.gets . view $ display . wlDisplay

waitReady :: (WithLog r, P.Members '[P.Writer [SWC.CallbackEvent], P.Input SWC.CallbackEvent, P.Reader ThreadId, P.Error SWC.FFIException] r)
          => P.Sem r Core -> P.Sem r Core
waitReady next = do
  event <- P.input
  logDebug ("Init: " % shown) (event)
  case event of
    SWC.Catastrophe { SWC.exception } -> do
      logError ("Error: " % string) $ displayException exception
      logDebug "Startup failed due to previous error"
      P.throw exception
    SWC.Ready { SWC.display, SWC.eventLoop } -> P.ask <&> \tid -> Core {
      _eventLoop = eventLoop,
      _display = Display display $ zipper IM.empty,
      _ffiThread = tid
    }
    _ -> P.tell [event] >> next

runCompositor :: (WithLog r, P.Members '[P.Embed IO] r) => P.Sem r ()
runCompositor = do
  init

  (tid, chan) <- P.embed @IO $ do
    tcOut <- newBroadcastTChanIO
    tcIn <- atomically $ dupTChan tcOut

    let bindTable = execWriter bindings :: M.Map SWC.Binding (IO ())

    controlThread <- forkOS . SWC.start tcOut $ M.keys bindTable
    pure (controlThread, tcIn)

  let stmToIO = P.runEmbedded @STM @IO atomically
  let withEventStream = P.runInputSem (P.embed $ readTChan chan)

  stmToIO . withEventStream $ do
    P.runError . flip (P.catch @SWC.FFIException) (const . forever $ P.input) $ do
      (events, core) <- P.runWriter . P.runReader tid $ fix waitReady
      mapM_ (P.embed . unGetTChan chan) $ reverse events
      P.runReader core $ loopDispatch
    forever $ P.input
  where
    init :: (WithLog r, P.Members '[P.Embed IO] r) => P.Sem r ()
    init = do
      logDebug   "_|          _|  _|      _|                                      _|"
      logInfo    "_|    _|    _|  _|_|  _|_|    _|_|    _|_|_|      _|_|_|    _|_|_|"
      logWarning "  _|  _|  _|    _|  _|  _|  _|    _|  _|    _|  _|    _|  _|    _|"
      logError   "    _|  _|      _|      _|    _|_|    _|    _|    _|_|_|    _|_|_|"
