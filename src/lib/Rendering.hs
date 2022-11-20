module Rendering (
  render
) where

import Control.Monad (when)
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
  (Aim ray _, player) <- get global
  let lineStart = addVectors (RL.ray'position ray) (Vector3 0 (-0.05) 0)
      lineEnd = addVectors (RL.ray'position ray) $
        multiplyVector (RL.ray'direction ray) 10
  liftIO $ RL.drawLine3D lineStart lineEnd $ playerColour player


renderBoards :: System World ()
renderBoards = do
  (Aim _ target, player) <- get global
  cmapM_ (renderBoard target player)


renderBoard :: LookAtTarget -> PlayerComponent ->
               (BoardComponent, PositionComponent, Entity) -> System World ()
renderBoard target player (b, Position p, e) = do
  renderCrosses p (b, e) target player
  liftIO $ do
    RL.drawCube (addVectors p $ Vector3 0.5    0 0) t 3 t RL.white
    RL.drawCube (addVectors p $ Vector3 (-0.5) 0 0) t 3 t RL.white
    RL.drawCube (addVectors p $ Vector3 0 0.5 0) 3 t t RL.white
    RL.drawCube (addVectors p $ Vector3 0 (-0.5) 0) 3 t t RL.white
  where t = 0.05


renderCrosses :: Vector3 -> (BoardComponent, Entity) -> LookAtTarget ->
                 PlayerComponent -> System World ()
renderCrosses origin (b, e) target player = do
  renderCross origin (-1)   1  (_tl b) (isAimingAtCell e 0 target) player
  renderCross origin   0    1  (_tc b) (isAimingAtCell e 1 target) player
  renderCross origin   1    1  (_tr b) (isAimingAtCell e 2 target) player
  renderCross origin (-1)   0  (_ml b) (isAimingAtCell e 3 target) player
  renderCross origin   0    0  (_mc b) (isAimingAtCell e 4 target) player
  renderCross origin   1    0  (_mr b) (isAimingAtCell e 5 target) player
  renderCross origin (-1) (-1) (_bl b) (isAimingAtCell e 6 target) player
  renderCross origin   0  (-1) (_bc b) (isAimingAtCell e 7 target) player
  renderCross origin   1  (-1) (_br b) (isAimingAtCell e 8 target) player


isAimingAtCell :: Entity -> Int -> LookAtTarget -> Bool
isAimingAtCell (Entity e) i (Target (Entity e') i') = e == e' && i == i'
isAimingAtCell _ _ _ = False


renderCross :: Vector3 -> Float -> Float -> Cell -> Bool -> PlayerComponent ->
               System World ()
renderCross _ _ _ Empty False _ = pure ()

renderCross origin i j (Filled player) _ _ = liftIO $ do
  RL.drawLine3D (f (-0.4) (-0.4)) (f 0.4 0.4) $ playerColour player
  RL.drawLine3D (f 0.4 (-0.4)) (f (-0.4) 0.4) $ playerColour player
  where center = addVectors origin $ Vector3 (CFloat i) (CFloat j) 0
        f x y = addVectors center $ Vector3 x y 0

renderCross origin i j Empty True player = liftIO $ do
    RL.drawCircle3D center 0.4 (Vector3 0 1 0) 0 $ playerColour player
  where center = addVectors origin $ Vector3 (CFloat i) (CFloat j) 0


playerColour :: PlayerComponent -> RL.Color
playerColour Red = RL.red
playerColour Blue = RL.skyBlue
