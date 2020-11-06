{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Graphics.Wayland.Server (
  module Graphics.Wayland.Server,
  module Graphics.Wayland.Server.Display,
  module Graphics.Wayland.Server.Events,
  module Graphics.Wayland.Server.Protocol
) where

import           Prelude
import           Graphics.Wayland.Server.Display
import           Graphics.Wayland.Server.Events
import           Graphics.Wayland.Server.Protocol

#include <wayland-server.h>

{#context prefix = "wl" #}
--{#enum KeyboardKeyState {underscoreToCase} deriving (Show, Eq) #}
