{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
module SWC (
  Screen(..),
  Window(..),
  BindingType(..),
  Modifier(..),
  Keysym(..),
  Binding(..),
  CallbackEvent(..),
  MonadWl(..),
  MonadSwc(..),
  FFIException(..),
  start
) where

import           Colog
import           Colog.Monad
import           Colog.Core.IO
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Exception
import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State
import           Control.Monad.Trans.Writer
import           Data.ByteString.Char8 as BS
import           Data.Foldable
import           Data.Int
import           Data.List as L
import           Data.List.Unique
import           Data.Time hiding (parseTime)
import           Data.Time.Clock.POSIX
import qualified Data.Text as T
import           Data.Typeable
import           Data.Word
import           Foreign.C.Types
import           Foreign.Marshal.Utils
import           Foreign.Ptr
import qualified Language.C.Inline as C
import           System.Posix.Env.ByteString
import           System.Posix.Signals
import           SWC.Internal hiding (ModifierMask)
import qualified SWC.Wayland as WL
import qualified System.IO as IO
import           System.IO.Unsafe
import           System.Posix.Signals
import           Text.XkbCommon
import           Text.XkbCommon.InternalTypes

{- Setup -}
C.context ctx

C.include "<math.h>"
C.include "<stdio.h>"

C.include "<stdlib.h>"
C.include "<swc.h>"
C.include "<unistd.h>"
C.include "<wayland-server.h>"
C.include "<xkbcommon/xkbcommon.h>"

{- Types -}
data CSourceLoc = CSourceLoc String [Argument]

data FFIException = NullPointerError CSourceLoc
                  | FailureResultError CSourceLoc
                  deriving (Typeable, Show)

instance Exception FFIException where
  displayException e@(NullPointerError loc) = execWriter $ do
    tell $ show (typeOf e)
    tell " at "
    tell $ show loc
  displayException e@(FailureResultError loc) = execWriter $ do
    tell $ show (typeOf e)
    tell " at "
    tell $ show loc

instance Show CSourceLoc where
  showsPrec _ (CSourceLoc func args) = let
    showArgs = L.foldr (.) id . L.intersperse (showString ", ") . fmap shows;
    in showString func . showParen True (showArgs args)

data Binding = Binding BindingType Modifier Keysym
             deriving (Eq, Show, Ord)

instance Ord Keysym where
  (<=) (Keysym a) (Keysym b) = a <= b

data CallbackEvent = Ready { display :: Ptr WL.Display, eventLoop :: Ptr WL.EventLoop }
                   | NewScreen { screen :: Ptr Screen }
                   | NewWindow { window :: Ptr Window }
                   | NewDevice { inputDevice :: Ptr InputDevice }
                   | SessionActive { activeState :: Bool }
                   | InputEvent { binding :: Binding, time :: UTCTime, keyValue :: Word32, keyState :: WL.KeyboardKeyState }
                   | SignalCaught { signal :: CInt }
                   | Stop { }
                   | Catastrophe { exception :: FFIException }
                   deriving (Show)

class Monad m => MonadWl m where
  terminate :: Ptr WL.Display -> m ()
  newDisplay :: m (Ptr WL.Display)
  addSocketAuto :: Ptr WL.Display -> m BS.ByteString
  eventLoopAddSignal :: Ptr WL.EventLoop -> CInt -> WL.SignalFunc -> m (Ptr WL.EventSource)
  getEventLoop :: Ptr WL.Display -> m (Ptr WL.EventLoop)
  runLoop :: Ptr WL.Display -> m ()
  destroyDisplay :: Ptr WL.Display -> m ()

  default terminate :: (MonadTrans t, MonadWl m', m ~ t m') => Ptr WL.Display -> m ()
  terminate = lift . terminate
  default newDisplay :: (MonadTrans t, MonadWl m', m ~ t m') => m (Ptr WL.Display)
  newDisplay = lift newDisplay
  default addSocketAuto :: (MonadTrans t, MonadWl m', m ~ t m') => Ptr WL.Display -> m BS.ByteString
  addSocketAuto = lift . addSocketAuto
  default getEventLoop :: (MonadTrans t, MonadWl m', m ~ t m') => Ptr WL.Display -> m (Ptr WL.EventLoop)
  getEventLoop = lift . getEventLoop
  default eventLoopAddSignal :: (MonadTrans t, MonadWl m', m ~ t m') => Ptr WL.EventLoop -> CInt -> WL.SignalFunc -> m (Ptr WL.EventSource)
  eventLoopAddSignal = ((lift .) .) . eventLoopAddSignal
  default runLoop :: (MonadTrans t, MonadWl m', m ~ t m') => Ptr WL.Display -> m ()
  runLoop = lift . runLoop
  default destroyDisplay :: (MonadTrans t, MonadWl m', m ~ t m') => Ptr WL.Display -> m ()
  destroyDisplay = lift . destroyDisplay

instance MonadWl IO where
  terminate display = [C.block| void {
    wl_display_terminate($(struct wl_display *display));
  } |]
  newDisplay = [C.exp| struct wl_display * {
    wl_display_create()
  } |] >>= ensurePtr "wl_display_create" []
  addSocketAuto display = [C.exp| const char * {
    wl_display_add_socket_auto($(struct wl_display *display))
  } |] >>= ensurePtr "wl_display_add_socket_auto" [arg display] >>= BS.packCString
  getEventLoop display = [C.exp| struct wl_event_loop * {
    wl_display_get_event_loop($(struct wl_display *display))
  } |] >>= ensurePtr "wl_display_get_event_loop" [arg display]
  eventLoopAddSignal eventLoop signal handler = [C.exp| struct wl_event_source * {
    wl_event_loop_add_signal($(struct wl_event_loop *eventLoop), $(int signal), $fun:(int (*handler)(int, void *)), NULL)
  } |] >>= ensurePtr "wl_event_loop_add_signal" [arg eventLoop, arg signal, Argstr "&handler"]
  runLoop display = [C.block| void {
    wl_display_run($(struct wl_display *display));
  } |]
  destroyDisplay display = [C.block| void {
    wl_display_destroy($(struct wl_display *display));
  } |]

instance MonadWl m => MonadWl (StateT s m)

class Monad m => MonadSwc m where
  screenSetHandler :: Ptr Screen -> Ptr ScreenHandler -> Ptr () -> m ()
  windowSetHandler :: Ptr Window -> Ptr WindowHandler -> Ptr () -> m ()
  windowClose :: Ptr Window -> m ()
  windowShow :: Ptr Window -> m ()
  windowHide :: Ptr Window -> m ()
  windowFocus :: Ptr Window -> m ()
  windowSetStacked :: Ptr Window -> m ()
  windowSetTiled :: Ptr Window -> m ()
  windowSetFullscreen :: Ptr Window -> Ptr Screen -> m ()
  windowSetPosition :: Ptr Window -> Int32 -> Int32 -> m ()
  windowSetSize :: Ptr Window -> Word32 -> Word32 -> m ()
  windowSetGeometry :: Ptr Window -> Ptr Rectangle -> m ()
  windowSetBorder :: Ptr Window -> Word32 -> Word32 -> m ()
  windowBeginMove :: Ptr Window -> m ()
  windowEndMove :: Ptr Window -> m ()
  windowBeginResize :: Ptr Window -> Word32 -> m ()
  windowEndResize :: Ptr Window -> m ()
  initialize :: Ptr WL.Display -> Ptr Manager -> m ()
  finalize :: m ()

  default screenSetHandler :: (MonadTrans t, MonadSwc m', m ~ t m') => Ptr Screen -> Ptr ScreenHandler -> Ptr () -> m ()
  screenSetHandler = ((lift .) .) . screenSetHandler
  default windowSetHandler :: (MonadTrans t, MonadSwc m', m ~ t m') => Ptr Window -> Ptr WindowHandler -> Ptr () -> m ()
  windowSetHandler = ((lift .) .) . windowSetHandler
  default windowClose :: (MonadTrans t, MonadSwc m', m ~ t m') => Ptr Window -> m ()
  windowClose = lift . windowClose
  default windowShow :: (MonadTrans t, MonadSwc m', m ~ t m') => Ptr Window -> m ()
  windowShow = lift . windowShow
  default windowHide :: (MonadTrans t, MonadSwc m', m ~ t m') => Ptr Window -> m ()
  windowHide = lift . windowHide
  default windowFocus :: (MonadTrans t, MonadSwc m', m ~ t m') => Ptr Window -> m ()
  windowFocus = lift . windowFocus
  default windowSetStacked :: (MonadTrans t, MonadSwc m', m ~ t m') => Ptr Window -> m ()
  windowSetStacked = lift . windowSetStacked
  default windowSetTiled :: (MonadTrans t, MonadSwc m', m ~ t m') => Ptr Window -> m ()
  windowSetTiled = lift . windowSetTiled
  default windowSetFullscreen :: (MonadTrans t, MonadSwc m', m ~ t m') => Ptr Window -> Ptr Screen -> m ()
  windowSetFullscreen = (lift .) . windowSetFullscreen
  default windowSetPosition :: (MonadTrans t, MonadSwc m', m ~ t m') => Ptr Window -> Int32 -> Int32 -> m ()
  windowSetPosition = ((lift .) .) . windowSetPosition
  default windowSetSize :: (MonadTrans t, MonadSwc m', m ~ t m') => Ptr Window -> Word32 -> Word32 -> m ()
  windowSetSize = ((lift .) .) . windowSetSize
  default windowSetGeometry :: (MonadTrans t, MonadSwc m', m ~ t m') => Ptr Window -> Ptr Rectangle -> m ()
  windowSetGeometry = (lift .) . windowSetGeometry
  default windowSetBorder :: (MonadTrans t, MonadSwc m', m ~ t m') => Ptr Window -> Word32 -> Word32 -> m ()
  windowSetBorder = ((lift .) .) . windowSetBorder
  default windowBeginMove :: (MonadTrans t, MonadSwc m', m ~ t m') => Ptr Window -> m ()
  windowBeginMove = lift . windowBeginMove
  default windowEndMove :: (MonadTrans t, MonadSwc m', m ~ t m') => Ptr Window -> m ()
  windowEndMove = lift . windowEndMove
  default windowBeginResize :: (MonadTrans t, MonadSwc m', m ~ t m') => Ptr Window -> Word32 -> m ()
  windowBeginResize = (lift .) . windowBeginResize
  default windowEndResize :: (MonadTrans t, MonadSwc m', m ~ t m') => Ptr Window -> m ()
  windowEndResize = lift . windowEndResize
  default initialize :: (MonadTrans t, MonadSwc m', m ~ t m') => Ptr WL.Display -> Ptr Manager -> m ()
  initialize = (lift .) . initialize
  default finalize :: (MonadTrans t, MonadSwc m', m ~ t m') => m ()
  finalize = lift finalize

instance MonadSwc IO where
  screenSetHandler screen handler ident = [C.block| void {
    swc_screen_set_handler($(struct swc_screen *screen), $(struct swc_screen_handler *handler), $(void *ident));
  } |]
  windowSetHandler window handler ident = [C.block| void {
    swc_window_set_handler($(struct swc_window *window), $(struct swc_window_handler *handler), $(void *ident));
  } |]
  windowClose window = [C.block| void {
    swc_window_close($(struct swc_window *window));
  } |]
  windowShow window = [C.block| void {
    swc_window_show($(struct swc_window *window));
  } |]
  windowHide window = [C.block| void {
    swc_window_hide($(struct swc_window *window));
  } |]
  windowFocus window = [C.block| void {
    swc_window_focus($(struct swc_window *window));
  } |]
  windowSetStacked window = [C.block| void {
    swc_window_set_stacked($(struct swc_window *window));
  } |]
  windowSetTiled window = [C.block| void {
    swc_window_set_tiled($(struct swc_window *window));
  } |]
  windowSetFullscreen window screen = [C.block| void {
    swc_window_set_fullscreen($(struct swc_window *window), $(struct swc_screen *screen));
  } |]
  windowSetPosition window x y = [C.block| void {
    swc_window_set_position($(struct swc_window *window), $(int32_t x), $(int32_t y));
  } |]
  windowSetSize window width height = [C.block| void {
    swc_window_set_size($(struct swc_window *window), $(uint32_t width), $(uint32_t height));
  } |]
  windowSetGeometry window geometry = [C.block| void {
    swc_window_set_geometry($(struct swc_window *window), $(struct swc_rectangle *geometry));
  } |]
  windowSetBorder window colour width = [C.block| void {
    swc_window_set_border($(struct swc_window *window), $(uint32_t colour), $(uint32_t width));
  } |]
  windowBeginMove window = [C.block| void {
    swc_window_begin_move($(struct swc_window *window));
  } |]
  windowEndMove window = [C.block| void {
    swc_window_end_move($(struct swc_window *window));
  } |]
  windowBeginResize window edges = [C.block| void {
    swc_window_begin_resize($(struct swc_window *window), $(uint32_t edges));
  } |]
  windowEndResize window = [C.block| void {
    swc_window_end_resize($(struct swc_window *window));
  } |]
  initialize display manager = [C.exp| bool {
    swc_initialize($(struct wl_display *display), NULL, $(struct swc_manager *manager))
  } |] >>= (\r -> case toBool r of
               True -> withLogger . logDebug . T.pack $ "swc_initialize(...) = " ++ show r;
               False -> throwFailureResult "swc_initialize" [arg display, arg manager])
  finalize = [C.block| void {
    swc_finalize();
  } |]

instance MonadSwc m => MonadSwc (StateT s m)

data Argument = forall a. Show a => Arg a | Argstr String

instance Show Argument where
  showsPrec p (Arg a) = showsPrec p a
  showsPrec _ (Argstr s) = showString s

{- Implementation -}
arg :: Show a => a -> Argument
arg = Arg

throwNullPointer :: String -> [Argument] -> IO a
throwNullPointer s a = throwIO . NullPointerError $ CSourceLoc s a

throwFailureResult :: String -> [Argument] -> IO a
throwFailureResult s a = throwIO . FailureResultError $ CSourceLoc s a

ensurePtr :: String -> [Argument] -> Ptr a -> IO (Ptr a)
ensurePtr source args ptr
  | ptr == nullPtr = do
      withLogger . logError . T.pack . show $ CSourceLoc source args
      throwNullPointer source args
  | otherwise      = do
      withLogger . logDebug . T.pack . execWriter $ do
        tell . show $ CSourceLoc source args
        tell " = "
        tell . show $ ptr
      return ptr

newScreenCallback :: (CallbackEvent -> IO ()) -> NewScreenCallback
newScreenCallback write = write . NewScreen

newWindowCallback :: (CallbackEvent -> IO ()) -> NewWindowCallback
newWindowCallback write = write . NewWindow

newDeviceCallback :: (CallbackEvent -> IO ()) -> NewDeviceCallback
newDeviceCallback write = write . NewDevice

sessionCallback :: Bool -> (CallbackEvent -> IO ()) -> SessionCallback
sessionCallback b write = write $ SessionActive b

addBinding :: (Num n) => BindingType -> Modifier -> Keysym -> BindingCallback -> Ptr () -> IO n
addBinding btyp modf key handler ptr = let
    t = (toEnum . fromEnum) btyp
    m = (toEnum . fromEnum) modf
    k = (unCKeysym . fromKeysym) key
  in fromIntegral <$> [C.exp| int {
    swc_add_binding((enum swc_binding_type)$(uint32_t t), $(uint32_t m), $(uint32_t k),
                    $fun:(void (*handler)(void *, uint32_t, uint32_t, uint32_t)), $(void *ptr))
  } |]

registerBinding :: (CallbackEvent -> IO ()) -> Binding -> IO Bool
registerBinding write r@(Binding b m k) = fmap (== 0) $ addBinding b m k handler nullPtr
  where
    parseTime :: Word32 -> UTCTime
    parseTime = posixSecondsToUTCTime . realToFrac

    parseState :: Word32 -> WL.KeyboardKeyState
    parseState = toEnum . fromEnum

    handler :: BindingCallback
    handler _ t v s = write $ InputEvent { binding = r, time = parseTime t, keyValue = v, keyState = parseState s }

newSignalCallback :: (CallbackEvent -> IO ()) -> WL.SignalFunc
newSignalCallback write n _ = write (SignalCaught n) >> pure 0

--start :: (Foldable t) => TChan CallbackEvent -> t Binding -> LoggerT [String] IO ()
start :: (Foldable t) => TChan CallbackEvent -> t Binding -> IO ()
start tcOut binds = let
    writer = atomically . writeTChan tcOut;
    onError = Handler $ \(ex :: FFIException) -> writer $ Catastrophe { exception = ex }
  in
    flip catches [onError] $ do
      _ <- installHandler sigUSR1 (Ignore) Nothing -- Prevent RTS misunderstandings

      display <- newDisplay

      let nscb = newScreenCallback writer
      let nwcb = newWindowCallback writer
      let ndcb = newDeviceCallback writer
      let sacb = sessionCallback True writer
      let sdcb = sessionCallback False writer
      manager <- mkManager nscb nwcb ndcb sacb sdcb

      socket <- addSocketAuto display
      setEnv "WAYLAND_DISPLAY" socket True
      withLogger . logInfo . T.pack $ "Socket: " ++ show socket

      initialize display manager -- Will clobber USR1 handler

      mapM_ (registerBinding writer) (sortUniq $ toList binds)

      eventLoop <- getEventLoop display
      let sccb = newSignalCallback writer
      _ <- eventLoopAddSignal eventLoop sigCHLD sccb
      writer $ Ready { display, eventLoop }

      runLoop display >> finalize
      writer $ Stop { }
  where
    mkManager :: NewScreenCallback -> NewWindowCallback -> NewDeviceCallback -> SessionCallback -> SessionCallback -> IO (Ptr Manager)
    mkManager ns nw nd sa sd = do
      fnNewScreen <- $(C.mkFunPtr [t| NewScreenCallback |]) ns
      fnNewWindow <- $(C.mkFunPtr [t| NewWindowCallback |]) nw
      fnNewDevice <- $(C.mkFunPtr [t| NewDeviceCallback |]) nd
      fnActivate <- $(C.mkFunPtr [t| SessionCallback |]) sa
      fnDeactivate <- $(C.mkFunPtr [t| SessionCallback |]) sd
      new $ Manager { fnNewScreen, fnNewWindow, fnNewDevice, fnActivate, fnDeactivate }

withLogger :: LoggerT Message IO a -> IO a
withLogger = usingLoggerT . liftLogIO . upgradeMessageAction defaultFieldMap $ cmapM fmtRichMessageDefault logTextStderr
