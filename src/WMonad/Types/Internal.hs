{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
module WMonad.Types.Internal where

import           Protolude
import           Control.Comonad.Store
import           Control.Lens
import           Data.Bifunctor (first)
import           Data.Function
import qualified Data.Map as M
import           Data.Maybe
import qualified Data.List.NonEmpty as NE
import           Foreign.Ptr
import qualified Graphics.Wayland.Server as WL
import qualified WMonad.SWC as SWC

type StoreMap k v = Context' (M.Map (Ptr k) v, Ptr k) (Maybe (Ptr k, v))

store :: Field1 a a b b => Lens' (Context' a s) b
store = locus . _1
focus :: Field2 a a b b => Lens' (Context' a s) b
focus = locus . _2

--focused :: (ComonadStore (M.Map (Ptr k) v, Ptr k) w) => IndexedLens' (Ptr k) (w (Maybe ((Ptr k), v))) (Maybe v)
--focused = ilens (first (fromMaybe nullPtr) . NE.unzip . extract . fst)
--  (\w a -> over _1 . seeks (fromMaybe id $ M.alter (const a) <$> (fmap fst . extract $ w)) $ w)
focused :: (Comonad w) => IndexedGetter (Ptr k) (w (Maybe (Ptr k, b))) (Maybe b)
focused = ito $ first (fromMaybe nullPtr) . NE.unzip . extract

data Core = Core { _eventLoop :: Ptr WL.EventLoop,
                   _display :: Display,
                   _ffiThread :: ThreadId }

data Display = Display { _wlDisplay :: Ptr WL.Display,
                         _screens :: StoreMap SWC.Screen Screen,
                         _windows :: StoreMap SWC.Window Window }

data Screen = Screen { }

data Window = Window { _currentScreen :: Ptr SWC.Screen }

makeLenses ''Core
makeLenses ''Display

makeClassy ''Screen
makeClassy ''Window
