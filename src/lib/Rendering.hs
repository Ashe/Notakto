module Rendering (
  render
) where

import Foreign.C.Types (CFloat(..))

import Apecs

import qualified Raylib as RL
import qualified Raylib.Colors as RL
import qualified Raylib.Constants as RL
import qualified Raylib.Types as RL
import Raylib.Types (Vector3 (..))

import Types
import Util

--------------------------------------------------------------------------------

render :: System World ()
render = do
  Camera camera <- get global
  liftIO $ do
    RL.beginDrawing
    RL.clearBackground RL.black
    RL.drawFPS 10 20
    RL.beginMode3D camera
    RL.drawGrid 20 1
  renderBoards
  renderAimRay
  liftIO $ do
    RL.endMode3D
    RL.endDrawing

--------------------------------------------------------------------------------

renderAimRay :: System World ()
renderAimRay = do
  Aim ray <- get global
  let lineStart = addVectors (RL.ray'position ray) (Vector3 0 (-0.05) 0)
      lineEnd = addVectors (RL.ray'position ray) $ multiplyVector (RL.ray'direction ray) 10
  liftIO $ RL.drawLine3D lineStart lineEnd RL.yellow


renderBoards :: System World ()
renderBoards = cmapM_ renderBoard


renderBoard :: (BoardComponent, PositionComponent) -> System World ()
renderBoard (b, Position p) = do
  renderCrosses p b
  liftIO $ do
    RL.drawCube (addVectors p $ Vector3 0.5    0 0) t 3 t RL.white
    RL.drawCube (addVectors p $ Vector3 (-0.5) 0 0) t 3 t RL.white
    RL.drawCube (addVectors p $ Vector3 0 0.5 0) 3 t t RL.white
    RL.drawCube (addVectors p $ Vector3 0 (-0.5) 0) 3 t t RL.white
  where t = 0.05


renderCrosses :: Vector3 -> BoardComponent -> System World ()
renderCrosses origin b = do
  renderCross origin (-1)   1  (_tl b)
  renderCross origin   0    1  (_tc b)
  renderCross origin   1    1  (_tr b)
  renderCross origin (-1)   0  (_ml b)
  renderCross origin   0    0  (_mc b)
  renderCross origin   1    0  (_mr b)
  renderCross origin (-1) (-1) (_bl b)
  renderCross origin   0  (-1) (_bc b)
  renderCross origin   1  (-1) (_br b)


renderCross :: Vector3 -> Float -> Float -> Cell -> System World ()
renderCross _ _ _ Empty = pure ()
renderCross origin i j Filled = liftIO $ do
  RL.drawLine3D (f (-0.4) (-0.4)) (f 0.4 0.4) RL.red
  RL.drawLine3D (f 0.4 (-0.4)) (f (-0.4) 0.4) RL.red
  where center = addVectors origin $ Vector3 (CFloat i) (CFloat j) 0
        f x y = addVectors center $ Vector3 x y 0
