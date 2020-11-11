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
module WMonad.Core (
  runCompositor
) where

import           Protolude hiding ((%))
import           Colog.Polysemy.Formatting
import           Control.Concurrent.STM
import           Control.Comonad.Store.Zipper
import           Control.Monad.Trans.Writer
import qualified Data.ByteString as BS
import           Data.Default
import           Data.Functor
import qualified Data.IntMap as IM
import qualified Data.Text as T
import           Data.Time.Clock
import           Control.Lens
import           Control.Lens.Prism
import qualified Data.Map as M
import           Formatting
import qualified Graphics.Wayland.Server as WL
import qualified Polysemy as P
import qualified Polysemy.Writer as P
import qualified Polysemy.Reader as P
import qualified Polysemy.Input as P
import qualified Polysemy.Embed as P
import qualified Polysemy.Final as P
import qualified Polysemy.Error as P
import qualified Polysemy.State as P
import qualified Polysemy.View as P
import qualified Text.XkbCommon as XKB
import qualified Text.XkbCommon.KeysymList as XKB
import qualified WMonad.SWC as SWC
import           WMonad.Types
import           WMonad.Types.Internal
import           WMonad.Events
import           WMonad.Events.InputEvent

bindings :: BindTable
bindings = M.fromList [
  (SWC.Binding SWC.BindingKey SWC.ModifierLogo XKB.keysym_Return, do
      pure {- spawn st-wl -} ()),
  (SWC.Binding SWC.BindingKey SWC.ModifierLogo XKB.keysym_r, do
      pure {- spawn dmenu_run-wl -} ()),
  (SWC.Binding SWC.BindingKey SWC.ModifierLogo XKB.keysym_q, do
      gets (view $ display.wlDisplay) >>= lift . SWC.withCall . WL.terminateDisplay)
  ]

spawnFFI :: (P.Member (P.Embed IO) r) => IO a -> P.Sem r ThreadId
spawnFFI = P.embed . forkOS . void

loopDispatch :: (WithLog r, P.Members '[P.Embed IO, P.View BindLookup, P.State Core, P.Input SWC.CallbackEvent] r) => P.Sem r ()
loopDispatch = do
  event <- P.input
  logDebug ("Event: " % shown) (event)
  case event of
    SWC.Catastrophe { SWC.exception } -> do
      logError ("Error: " % string) $ displayException exception
      logDebug "Exiting due to previous error"
      P.gets (view $ display.wlDisplay) >>= void . spawnFFI . SWC.withCall . WL.terminateDisplay
    SWC.Ready _ _ -> undefined
    SWC.InputEvent { SWC.binding, SWC.time, SWC.keyValue, SWC.keyState } -> do
      handleInput binding time keyValue keyState >> loopDispatch
    _ -> loopDispatch -- undefined

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
      _display = Display {
        _wlDisplay = display,
        _screens = def,
        _windows = def
      },
      _ffiThread = tid
    }
    _ -> P.tell [event] >> next

runCompositor :: (WithLog r, P.Members '[P.Embed IO] r) => P.Sem r ()
runCompositor = do
  init

  (tid, chan) <- P.embed @IO $ do
    tcOut <- newBroadcastTChanIO
    tcIn <- atomically $ dupTChan tcOut

    controlThread <- forkOS . SWC.start tcOut $ M.keys bindings
    pure (controlThread, tcIn)

  let stmToIO = P.runEmbedded @STM @IO atomically
  let withEventStream = P.runInputSem (P.embed $ readTChan chan)
  let withBindings = P.runInputConst bindings . P.viewToInput (flip M.lookup)

  stmToIO . withEventStream $ do
    _ <- P.runError . flip (P.catch @SWC.FFIException) (const . forever $ P.input) $ do
      (events, core) <- P.runWriter . P.runReader tid $ fix waitReady
      mapM_ (P.embed . unGetTChan chan) $ reverse events
      withBindings . P.runState @Core core $ loopDispatch
    forever $ P.input
  where
    init :: (WithLog r, P.Members '[P.Embed IO] r) => P.Sem r ()
    init = do
      logDebug   "_|          _|  _|      _|                                      _|"
      logInfo    "_|    _|    _|  _|_|  _|_|    _|_|    _|_|_|      _|_|_|    _|_|_|"
      logWarning "  _|  _|  _|    _|  _|  _|  _|    _|  _|    _|  _|    _|  _|    _|"
      logError   "    _|  _|      _|      _|    _|_|    _|    _|    _|_|_|    _|_|_|"
