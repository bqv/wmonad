{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Graphics.Wayland.Server.Events (
  Callback(..),
  EventSourceResult(..),
  EventSource,
  EventLoop,
  EventLoopSignalCallback,
  EventLoopSignalFunc,
  FunPtr,
  eventLoopAddSignal
) where

import           Prelude
import           Data.Enum.Util
import           Foreign.Callable
import           Foreign.Callback
import           Foreign.C.Types
import           Foreign.Ptr
import           Foreign.Ptr.Total
import           System.Posix.Signals

#include <wayland-server.h>
#include <wayland-server-core.h>

{#context prefix = "wl" #}

data EventLoop
{#pointer *wl_event_loop as EventLoop_ -> EventLoop #}

data EventSource
{#pointer *wl_event_source as EventSource_ -> Ptr EventSource #}

type EventLoopSignalCallback = Signal -> IO EventSourceResult
type EventLoopSignalFunc = CInt -> Ptr () -> IO CInt
foreign import ccall "wrapper"
  _wrapSignalFunc :: EventLoopSignalFunc -> IO (FunPtr EventLoopSignalFunc)
instance Callback EventLoopSignalCallback EventLoopSignalFunc where
  wrapF f = _wrapSignalFunc $ \n _ -> toEnum . fromEnum <$> f (fromIntegral n)
instance {-# OVERLAPPING #-} Arg EventLoopSignalCallback where
  arg _ = ArgStr "&callback"

eventLoopAddSignal = traceC3 "wl_event_loop_add_signal" eventLoopAddSignal'
eventLoopAddSignal' :: Ptr EventLoop -> Signal -> EventLoopSignalCallback -> IO (Maybe (Ptr EventSource))
eventLoopAddSignal' e s c = wrapF c >>= \h -> eventLoopAddSignal'' e s h nullPtr
{#fun wl_event_loop_add_signal as eventLoopAddSignal''
  { id `Ptr EventLoop', mapEnum `Signal', id `FunPtr EventLoopSignalFunc', `Ptr ()' } -> `Maybe (Ptr EventSource)' safeCastPtr #}

data EventSourceResult = EventSourceDone | EventSourceRecheck deriving (Eq, Show)
instance Enum EventSourceResult where
  fromEnum EventSourceDone = 0
  fromEnum EventSourceRecheck = 1
  toEnum _ = undefined
