{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
module SWC.Wayland where

data Display
data EventLoop

{- Constants -}
#include <wayland-server-protocol.h>
data KeyboardKeyState = KeyboardKeyStatePressed
                      | KeyboardKeyStateReleased
                      deriving (Eq, Show)

instance Enum KeyboardKeyState where
  fromEnum KeyboardKeyStatePressed = #{const WL_KEYBOARD_KEY_STATE_PRESSED}
  fromEnum KeyboardKeyStateReleased = #{const WL_KEYBOARD_KEY_STATE_RELEASED}
  toEnum #{const WL_KEYBOARD_KEY_STATE_PRESSED} = KeyboardKeyStatePressed
  toEnum #{const WL_KEYBOARD_KEY_STATE_RELEASED} = KeyboardKeyStateReleased
  toEnum _ = error "Bad value for WlKeyboardKeyState"
