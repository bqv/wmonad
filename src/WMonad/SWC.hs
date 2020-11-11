{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DuplicateRecordFields #-}
module WMonad.SWC (
  Screen(..),
  Window(..),
  Binding(..),
  BindingType(..),
  Modifier(..),
  Keysym(..),
  CallbackEvent(..),
  FFIException(..),
  new, with,
  withCall,
  start
) where

import           Protolude
import           Colog
import           Control.Concurrent.STM
import           Control.Monad
import qualified Control.Monad.Morph as MM
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Writer hiding (writer)
import           Data.ByteString.Char8 as BS
import           Data.List as L
import           Data.List.Unique
import qualified Data.Map as Map
import           Data.Time
import           Data.Text (Text)
import qualified Data.Text as T
import           Foreign.Callable
import           Foreign.Marshal.Utils
import           Foreign.Ptr
import           Graphics.Wayland.SWC
import           Graphics.Wayland.SWC.Types
import qualified Graphics.Wayland.Server as WL
import           System.Posix.Env.ByteString
import           System.IO
import           System.InputDevice
import           System.Posix.Signals
import           TextShow
import           TextShow.Data.Char
import           Text.Show hiding (show)
import           Text.XkbCommon

data CSourceLoc = CSourceLoc [Char] [ShowableArg]

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
  showsPrec p x = showsPrec p $ FromTextShow x

instance TextShow CSourceLoc where
  showbPrec _ (CSourceLoc func args) = let
    showArgs = mconcat . L.intersperse (showbLitString ", ") . fmap showb;
    in showbLitString func <> showbParen True (showArgs args)

data Binding = Binding BindingType Modifier Keysym
             deriving (Eq, Show, Ord)

instance Ord Keysym where
  (<=) (Keysym a) (Keysym b) = a <= b

data CallbackEvent = Ready { display :: Ptr WL.Display, eventLoop :: Ptr WL.EventLoop }
                   | NewScreen { screen :: Ptr Screen }
                   | NewWindow { window :: Ptr Window }
                   | NewDevice { inputDevice :: InputDevice }
                   | SessionActive { activeState :: Bool }
                   | InputEvent { binding :: Binding, time :: UTCTime, keyValue :: Keysym, keyState :: WL.KeyboardKeyState }
                   | ScreenEvent { screen :: Ptr Screen, screenEvent :: ScreenCallbackEvent }
                   | WindowEvent { window :: Ptr Window, windowEvent :: WindowCallbackEvent }
                   | SignalCaught { signal :: Signal }
                   | Stop { }
                   | Catastrophe { exception :: FFIException }
                   deriving (Show)

data ScreenCallbackEvent = ScreenDestroy
                         | GeometryChanged
                         | UsableGeometryChanged
                         | ScreenEntered
                         deriving (Show)

data WindowCallbackEvent = WindowDestroy
                         | TitleChanged
                         | AppIdChanged
                         | ParentChanged
                         | WindowEntered
                         | WindowMove
                         | WindowResize
                         deriving (Show)

newScreenCallback :: Ptr ScreenHandler -> (CallbackEvent -> IO ()) -> NewScreenCallback
newScreenCallback handler write screen = do
  withCall $ screenSetHandler screen handler
  write $ NewScreen screen

newWindowCallback :: Ptr WindowHandler -> (CallbackEvent -> IO ()) -> NewWindowCallback
newWindowCallback handler write window = do
  withCall $ windowSetHandler window handler
  write $ NewWindow window

newDeviceCallback :: (CallbackEvent -> IO ()) -> NewDeviceCallback
newDeviceCallback write = write . NewDevice

sessionCallback :: Bool -> (CallbackEvent -> IO ()) -> SessionCallback
sessionCallback b write = write $ SessionActive b

registerBinding :: (CallbackEvent -> IO ()) -> Binding -> IO Bool
registerBinding write r@(Binding b m k) = fmap (== 0) . withCall $ addBinding b m k handler
  where
    handler :: BindingCallback
    handler t v s = write $ InputEvent { binding = r, time = t, keyValue = v, keyState = s }

newSignalCallback :: (CallbackEvent -> IO ()) -> WL.EventLoopSignalCallback
newSignalCallback write n = write (SignalCaught n) >> pure WL.EventSourceDone

screenEventCallback :: ScreenCallbackEvent -> (CallbackEvent -> IO ()) -> DataCallback Screen
screenEventCallback screenEvent write screen = write $ ScreenEvent { screen, screenEvent }

windowEventCallback :: WindowCallbackEvent -> (CallbackEvent -> IO ()) -> DataCallback Window
windowEventCallback windowEvent write window = write $ WindowEvent { window, windowEvent }

start :: (Foldable t) => TChan CallbackEvent -> t Binding -> IO ()
start tcOut binds = let
    writer = atomically . writeTChan tcOut;
    onError = Handler $ \(ex :: FFIException) -> writer $ Catastrophe { exception = ex }
  in
    flip catches [onError] $ do
      _ <- installHandler sigUSR1 (Ignore) Nothing -- Prevent RTS misunderstandings

      display <- withMaybeCall $ WL.createDisplay

      screenHandler <- mkScreenHandler writer
      windowHandler <- mkWindowHandler writer
      manager <- mkManager screenHandler windowHandler writer

      socket <- withMaybeCall $ WL.addDisplaySocketAuto display
      setEnv "WAYLAND_DISPLAY" (BS.pack socket) True
      withLogger . logInfo . T.pack $ "Socket: " ++ socket

      withCall $ initialize display manager -- Will clobber USR1 handler

      mapM_ (registerBinding writer) (sortUniq $ toList binds)

      eventLoop <- withMaybeCall $ WL.getDisplayEventLoop display
      let sccb = newSignalCallback writer
      _ <- withMaybeCall $ WL.eventLoopAddSignal eventLoop sigCHLD sccb
      writer $ Ready { display, eventLoop }

      withCall $ WL.runDisplay display
      withCall $ finalize
      writer $ Stop { }
  where
    mkManager :: Ptr ScreenHandler -> Ptr WindowHandler -> (CallbackEvent -> IO ()) -> IO (Ptr Manager)
    mkManager screenHandler windowHandler writer = do
      let fnNewScreen = newScreenCallback screenHandler writer
      let fnNewWindow = newWindowCallback windowHandler writer
      let fnNewDevice = newDeviceCallback writer
      let fnActivate = sessionCallback True writer
      let fnDeactivate = sessionCallback False writer
      new $ Manager { fnNewScreen, fnNewWindow, fnNewDevice, fnActivate, fnDeactivate }

    mkScreenHandler :: (CallbackEvent -> IO ()) -> IO (Ptr ScreenHandler)
    mkScreenHandler writer = do
      let fnScreenDestroy = screenEventCallback ScreenDestroy writer
      let fnGeometryChanged = screenEventCallback GeometryChanged writer
      let fnUsableGeometryChanged = screenEventCallback UsableGeometryChanged writer
      let fnScreenEntered = screenEventCallback ScreenEntered writer
      new $ ScreenHandler { fnScreenDestroy, fnGeometryChanged, fnUsableGeometryChanged, fnScreenEntered }

    mkWindowHandler :: (CallbackEvent -> IO ()) -> IO (Ptr WindowHandler)
    mkWindowHandler writer = do
      let fnWindowDestroy = windowEventCallback WindowDestroy writer
      let fnTitleChanged = windowEventCallback TitleChanged writer
      let fnAppIdChanged = windowEventCallback AppIdChanged writer
      let fnParentChanged = windowEventCallback ParentChanged writer
      let fnWindowEntered = windowEventCallback WindowEntered writer
      let fnWindowMove = windowEventCallback WindowMove writer
      let fnWindowResize = windowEventCallback WindowResize writer
      new $ WindowHandler { fnWindowDestroy, fnTitleChanged, fnAppIdChanged, fnParentChanged, fnWindowEntered, fnWindowMove, fnWindowResize }

withMaybeCall :: HasCallStack => WriterT [Call] IO (Maybe b) -> IO b
withMaybeCall w = withFrozenCallStack $ do
  (r, ls) <- runWriterT w
  case r of
    Just x -> mapM_ (withLogger . logDebug . showCall) ls >> pure x
    Nothing -> mapM_ (fail . T.unpack . showCall) ls >> pure (panic mempty)

withCall :: HasCallStack => WriterT [Call] IO b -> IO b
withCall w = withFrozenCallStack $ do
  (r, ls) <- runWriterT w
  mapM_ (withLogger . logDebug . showCall) ls
  pure r

showCall :: ([Char], [ShowableArg], ShowableRes) -> Text
showCall (s, a, r) = execWriter $ do
  tell . T.pack . flip (showsPrec 0) "" $ CSourceLoc s a
  tell $ " = "
  tell . T.pack . flip (showsPrec 0) "" $ r

withLogger :: LoggerT Message IO a -> IO a
withLogger = usingLoggerT . liftLogIO . upgradeMessageAction defaultFieldMap $ cmapM fmtRichMessageDefault logTextStderr
