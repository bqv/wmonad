{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
module WMonad.Events.NewWindow (
  handleNewWindow
) where

import           Protolude
import           Colog.Polysemy.Formatting
import           Control.Category ((>>>))
import           Control.Comonad
import           Control.Comonad.Store.Zipper
import           Control.Exception
import           Control.Lens
import           Control.Monad.Trans.State
import           Data.Colour
import           Data.Colour.SRGB
import qualified Data.Map as M
import           Data.Time.Clock
import qualified Graphics.Wayland.Server as WL
import qualified Graphics.Wayland.SWC.Types as SWC
import qualified Graphics.Wayland.SWC as SWC
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
import           WMonad.Types.Internal

arrange :: (WithLog r, P.Members '[P.State Core, P.Embed IO] r)
        => P.Sem r ()
arrange = do
  screen <- P.gets @Core $ iview (display.screens.focused)
  windows <- P.gets @Core $ view (display.windows.store)
    >>> itoListOf (ifolded.filtered ((/= view _1 screen) . view currentScreen))
  screenGeom <- P.embed $ screen ^. _1 . peeked <&> SWC.screenUsableGeometry
  SWC.Rectangle {
    SWC.posX, SWC.sizeW, SWC.posY, SWC.sizeH
  } <- P.embed $ screenGeom ^. peeked
  case length windows of
    0 -> pure ()
    num -> P.embed @IO $ do
      let cols = num & ceiling . sqrt . fromIntegral
      let rows = fromIntegral num `div` 3 & (+ 1)
      sequence_ . zipWith (&) windows $ do
        ncol <- [0..cols-1]
        nrow <- [0..rows-1]
        let i = (ncol * cols) + nrow
        guard (i < num)

        let winGeom = SWC.Rectangle {
          SWC.posX = posX + borderWidth + ((fromIntegral sizeH) * ncol `div` cols),
          SWC.sizeW = fromIntegral $ (posX `div` cols) - (2 * borderWidth),
          SWC.posY = posY + borderWidth + ((fromIntegral sizeW) * nrow `div` rows),
          SWC.sizeH = fromIntegral $ (posY `div` rows) - (2 * borderWidth)
        }

        return $ \win ->
          SWC.with winGeom $ SWC.withCall . SWC.windowSetGeometry (win ^. _1)
{-
  /* This is a basic grid arrange function that tries to give each window an
   * equal space. */
  void *window = NULL;
  unsigned num_columns, num_rows, column_index, row_index;
  struct swc_rectangle geometry;
  struct swc_rectangle *screen_geometry = &$(void *p_screen)->swc->usable_geometry

  if ($(void *p_screen)->num_windows == 0)
    return;

  num_columns = ceil(sqrt($(void *p_screen)->num_windows));
  num_rows = $(void *p_screen)->num_windows / num_columns + 1;
  window = wl_container_of($(void *p_screen)->windows.next, window, link);

  for (column_index = 0; &window->link != &$(void *p_screen)->windows; ++column_index) {
    geometry.x = screen_geometry->x + border_width + screen_geometry->width * column_index / num_columns;
    geometry.width = screen_geometry->width / num_columns - 2 * border_width;

    if (column_index == $(void *p_screen)->num_windows % num_columns)
      num_rows--;

    for (row_index = 0; row_index < num_rows; ++row_index) {
      geometry.y = screen_geometry->y + border_width + screen_geometry->height * row_index / num_rows;
      geometry.height = screen_geometry->height / num_rows - 2 * border_width;

      swc_window_set_geometry(window->swc, &geometry);
      window = wl_container_of(window->link.next, window, link);
    }
  }
-}

borderWidth :: Integral n => n
borderWidth = 1
borderActive :: (Floating n, Ord n) => Colour n
borderActive = sRGB24read "#333388"
borderNormal :: (Floating n, Ord n) => Colour n
borderNormal = sRGB24 0x88 0x88 0x88 -- Data.Colour.Names.grey

focusWindow :: (WithLog r, P.Members '[P.State Core, P.Embed IO] r)
            => Ctx Window -> P.Sem r ()
focusWindow windowCtx = do
  current' <- P.gets @Core $ ipreview (display.windows.focused._Just)
  let target' = windowCtx & ipreview (focused._Just)

  P.embed (unfocus current' >> focus target')

  P.modify @Core $ set (display.windows) windowCtx
--- if (focused_window) {
---   swc_window_set_border(focused_window->swc, border_color_normal, border_width);
--- }
--- if ($(void *p_window)) {
---   swc_window_set_border($(void *p_window)->swc, border_color_active, border_width);
---   swc_window_focus($(void *p_window)->swc);
--- } else
---   swc_window_focus(NULL);
--- focused_window = $(void *p_window);
  where
    unfocus (Just p) = do
      SWC.withCall $ SWC.windowSetBorder (p ^. _1) borderNormal borderWidth
    unfocus Nothing = pure ()
    focus (Just p) = do
      SWC.withCall $ SWC.windowSetBorder (p ^. _1) borderActive borderWidth
      SWC.withCall $ SWC.windowFocus $ Just (p ^. _1)
    focus Nothing = do
      SWC.withCall $ SWC.windowFocus Nothing

-- @@swc_add_window@@
--- $(void *p_window)->screen = $(void *p_screen);
--- wl_list_insert(&($(void *p_screen)->windows), &($(void *p_window)->link));
--- $(void *p_screen)->num_windows++;
--- swc_window_show($(void *p_window)->swc);
--- arrange($(void *p_screen));

handleNewWindow :: (WithLog r, P.Members '[P.State Core, P.Embed IO] r)
                => CtxPtr Window -> P.Sem r ()
handleNewWindow ptrWindow = do
  activeScreen' <- P.gets @Core $ ipreview (display.screens.focused._Just)
  activeScreen <- maybe (panic "no screen") pure activeScreen'
  let window = Window { _currentScreen = activeScreen ^. _1 }

  P.modify @Core $ set (display.windows.store.at ptrWindow) $ Just window
  P.embed @IO (SWC.withCall $ SWC.windowSetTiled ptrWindow)
  P.embed @IO (SWC.withCall $ SWC.windowShow ptrWindow)
  arrange >> P.get @Core >>= focusWindow . set (focus) ptrWindow . view (display.windows)
--- swc_window_set_handler($(struct swc_window *p_swc), $(struct swc_window_handler *wh), $(void *p_window));
--- swc_window_set_tiled($(struct swc_window *p_swc));
--- screen_add_window(active_screen, $(void *p_window));
--- focus($(void *p_window));
