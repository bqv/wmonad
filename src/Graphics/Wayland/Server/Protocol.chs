module Graphics.Wayland.Server.Protocol where

import           Prelude
import           Foreign.C.Types
import           Foreign.Ptr

#include <wayland-server-protocol.h>

{#context prefix = "wl" #}

{#enum define KeyboardKeyState {
    WL_KEYBOARD_KEY_STATE_PRESSED as KeyboardKeyStatePressed,
    WL_KEYBOARD_KEY_STATE_RELEASED as KeyboardKeyStateReleased}
deriving (Eq, Show) #}
