{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
module WMonad.Events.InputEvent (
  BindOp,
  BindTable,
  BindLookup,
  handleInput
) where

import           Protolude
import           Colog.Polysemy.Formatting
import           Control.Comonad.Store.Zipper
import           Control.Monad.Trans.State
import qualified Data.Map as M
import           Data.Time.Clock
import qualified Graphics.Wayland.Server as WL
import qualified Polysemy as P
import qualified Polysemy.Writer as P
import qualified Polysemy.Reader as P
import qualified Polysemy.Input as P
import qualified Polysemy.Embed as P
import qualified Polysemy.Error as P
import qualified Polysemy.State as P
import qualified Polysemy.View as P
import qualified Text.XkbCommon as XKB
import qualified WMonad.SWC as SWC
import           WMonad.Types

{-
spawn :: SWC.BindingCallback
spawn p_data time value state
  | (toEnum.fromEnum) state == WL.KeyboardKeyStatePressed = do
    [C.block| void {
      char *const *command = $(void *p_data);

      if (fork() == 0) {
        execvp(command[0], command);
        exit(EXIT_FAILURE);
      }
    } |]
  | otherwise = pure ()
-}

type BindOp = StateT Core IO ()
type BindTable = M.Map SWC.Binding BindOp
type BindLookup = SWC.Binding -> Maybe BindOp

handleInput :: (WithLog r, P.Members '[P.View BindLookup, P.State Core, P.Embed IO] r)
            => SWC.Binding -> UTCTime -> XKB.Keysym -> WL.KeyboardKeyState -> P.Sem r ()
handleInput binding time keyValue keyState
  | keyState == WL.KeyboardKeyStatePressed = do
      handler <- P.see @BindLookup <*> pure binding
      core <- P.get @Core
      res <- P.embed . sequence $ flip runStateT core <$> handler
      (flip $ maybe (pure ())) res $ \res -> do
        P.put @Core $ snd res
        pure $ fst res
handleInput _ _ _ _ = pure ()
