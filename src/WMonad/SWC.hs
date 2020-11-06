{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
module WMonad.SWC (
  Screen(..),
  Window(..),
  Binding(..),
  BindingType(..),
  Modifier(..),
  Keysym(..),
  CallbackEvent(..),
  FFIException(..),
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
                   | SignalCaught { signal :: Signal }
                   | Stop { }
                   | Catastrophe { exception :: FFIException }
                   deriving (Show)

newScreenCallback :: (CallbackEvent -> IO ()) -> NewScreenCallback
newScreenCallback write = write . NewScreen

newWindowCallback :: (CallbackEvent -> IO ()) -> NewWindowCallback
newWindowCallback write = write . NewWindow

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

start :: (Foldable t) => TChan CallbackEvent -> t Binding -> IO ()
start tcOut binds = let
    writer = atomically . writeTChan tcOut;
    onError = Handler $ \(ex :: FFIException) -> writer $ Catastrophe { exception = ex }
  in
    flip catches [onError] $ do
      _ <- installHandler sigUSR1 (Ignore) Nothing -- Prevent RTS misunderstandings

      display <- withMaybeCall $ WL.createDisplay

      let nscb = newScreenCallback writer
      let nwcb = newWindowCallback writer
      let ndcb = newDeviceCallback writer
      let sacb = sessionCallback True writer
      let sdcb = sessionCallback False writer
      manager <- mkManager nscb nwcb ndcb sacb sdcb

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
    mkManager :: NewScreenCallback -> NewWindowCallback -> NewDeviceCallback -> SessionCallback -> SessionCallback -> IO (Ptr Manager)
    mkManager fnNewScreen fnNewWindow fnNewDevice fnActivate fnDeactivate = do
      new $ Manager { fnNewScreen, fnNewWindow, fnNewDevice, fnActivate, fnDeactivate }

withMaybeCall :: HasCallStack => WriterT [Call] IO (Maybe b) -> IO b
withMaybeCall w = withFrozenCallStack $ do
  (r, ls) <- runWriterT w
  case r of
    Just x -> mapM_ (withLogger . logDebug . showCall) ls >> pure x
    Nothing -> fail . mconcat $ fmap (T.unpack . showCall) ls

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
