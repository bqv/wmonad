{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE NamedFieldPuns #-}
module SWC (
  start
) where

import           Control.Concurrent.STM
import           Control.Monad.State
import           Data.ByteString as BS
import           Data.List.Unique
import           Data.Time hiding (parseTime)
import           Data.Time.Clock.POSIX
import           Data.Word
import           Foreign.C.Types
import           Foreign.C.String
import           Foreign.Marshal.Utils
import           Foreign.Ptr
import qualified Language.C.Inline as C
import           System.Posix.Env.ByteString
import           SWC.Internal hiding (ModifierMask)
import qualified SWC.Wayland as WL
import           Text.XkbCommon
import           Text.XkbCommon.InternalTypes

C.context ctx

{- Includes -}
C.include "<math.h>"
C.include "<stdio.h>"

C.include "<stdlib.h>"
C.include "<swc.h>"
C.include "<unistd.h>"
C.include "<wayland-server.h>"
C.include "<xkbcommon/xkbcommon.h>"

{- Implementation -}
ensurePtr :: (MonadFail m) => String -> Ptr a -> m (Ptr a)
ensurePtr errMsg ptr
  | ptr == nullPtr = return ptr
  | otherwise      = fail errMsg

data Binding = Binding BindingType Modifier Keysym
             deriving (Eq, Show, Ord)

instance Ord Keysym where
  (<=) (Keysym a) (Keysym b) = a <= b

data CallbackEvent = Ready (Ptr WL.Display) (Ptr WL.EventLoop)
                   | NewScreen (Ptr Screen)
                   | NewWindow (Ptr Window)
                   | InputEvent Binding UTCTime Word32 WL.KeyboardKeyState
                   deriving (Show)

newScreenCallback :: (CallbackEvent -> IO ()) -> NewScreenCallback
newScreenCallback write = write . NewScreen

newWindowCallback :: (CallbackEvent -> IO ()) -> NewWindowCallback
newWindowCallback write = write . NewWindow

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
    handler _ t v s = write $ InputEvent r (parseTime t) v (parseState s)

start :: TChan CallbackEvent -> [Binding] -> IO ()
start tcOut binds = do
  let writer = atomically . writeTChan tcOut

  display <- newDisplay

  manager <- mkManager (newScreenCallback writer) (newWindowCallback writer)

  socket <- addSocket display >>= BS.packCString
  setEnv "WAYLAND_DISPLAY" socket True

  initialize display manager

  mapM_ (registerBinding writer) (sortUniq binds)

  eventLoop <- getEventLoop display
  writer $ Ready display eventLoop

  runLoop display >> finalize display
  where
    mkManager :: NewScreenCallback -> NewWindowCallback -> IO (Ptr Manager)
    mkManager ns nw = do
      fnNewScreen <- $(C.mkFunPtr [t| NewScreenCallback |]) ns
      fnNewWindow <- $(C.mkFunPtr [t| NewWindowCallback |]) nw
      new $ Manager { fnNewScreen, fnNewWindow }

    newDisplay :: IO (Ptr WL.Display)
    newDisplay = [C.exp| struct wl_display * {
      wl_display_create()
    } |] >>= ensurePtr "error: wl_display_create"

    addSocket :: Ptr WL.Display -> IO CString
    addSocket p_display = [C.exp| const char * {
      wl_display_add_socket_auto($(struct wl_display *p_display))
    } |] >>= ensurePtr "error: wl_display_add_socket_auto"

    initialize :: Ptr WL.Display -> Ptr Manager -> IO ()
    initialize p_display p_manager = [C.exp| bool {
      swc_initialize($(struct wl_display *p_display), NULL, $(struct swc_manager *p_manager))
    } |] >>= (flip unless $ fail "error: swc_initialize") . toBool

    getEventLoop :: Ptr WL.Display -> IO (Ptr WL.EventLoop)
    getEventLoop p_display = [C.exp| struct wl_event_loop * {
      wl_display_get_event_loop($(struct wl_display *p_display))
    } |] >>= ensurePtr "error: wl_display_get_event_loop"

    runLoop :: Ptr WL.Display -> IO ()
    runLoop p_display = [C.block| void {
      wl_display_run($(struct wl_display *p_display));
    } |]

    finalize :: Ptr WL.Display -> IO ()
    finalize p_display = [C.block| void {
      wl_display_destroy($(struct wl_display *p_display));
    } |]
