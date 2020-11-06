{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Graphics.Wayland.Server.Display where

import           Prelude
import           Data.ByteString (ByteString, packCString)
import           Foreign.Callable
import           Foreign.C.String
import           Foreign.Ptr
import           Foreign.Ptr.Total
import           Graphics.Wayland.Server.Events
import           Text.Show

#include <wayland-server.h>

data Display
{#pointer *wl_display as Display_ -> Display #}

terminateDisplay = traceC1 "wl_display_terminate" terminateDisplay'
terminateDisplay' :: Ptr Display -> IO ()
terminateDisplay' = terminateDisplay''
{#fun wl_display_terminate as terminateDisplay''
  { id `Ptr Display' } -> `()' #}

createDisplay = traceC0 "wl_display_create" createDisplay'
createDisplay' :: IO (Maybe (Ptr Display))
createDisplay' = createDisplay''
{#fun wl_display_create as createDisplay''
  { } -> `Maybe (Ptr Display)' safeCastPtr #}

addDisplaySocketAuto = traceC1 "wl_display_add_socket_auto" addDisplaySocketAuto'
addDisplaySocketAuto' :: Ptr Display -> IO (Maybe String)
addDisplaySocketAuto' d = addDisplaySocketAuto'' d >>= sequence . fmap peekCString
{#fun wl_display_add_socket_auto as addDisplaySocketAuto''
  { id `Ptr Display' } -> `Maybe CString' safeCastPtr #}

getDisplayEventLoop = traceC1 "wl_display_get_event_loop" getDisplayEventLoop'
getDisplayEventLoop' :: Ptr Display -> IO (Maybe (Ptr EventLoop))
getDisplayEventLoop' = getDisplayEventLoop''
{#fun wl_display_get_event_loop as getDisplayEventLoop''
  { id `Ptr Display' } -> `Maybe (Ptr EventLoop)' safeCastPtr #}

runDisplay = traceC1 "wl_display_run" runDisplay'
runDisplay' :: Ptr Display -> IO ()
runDisplay' = runDisplay''
{#fun wl_display_run as runDisplay''
  { id `Ptr Display' } -> `()' #}

destroyDisplay = traceC1 "wl_display_destroy" destroyDisplay'
destroyDisplay' :: Ptr Display -> IO ()
destroyDisplay' = destroyDisplay''
{#fun wl_display_destroy as destroyDisplay''
  { id `Ptr Display' } -> `()' #}
