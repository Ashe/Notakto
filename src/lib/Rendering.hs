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
renderBoards = do
  numBoards <- cfold (\c (Board{}) -> c + 1) 0
  cfoldM_ (renderBoard numBoards) 0


renderBoard :: Int -> Int -> BoardComponent -> System World Int
renderBoard total i b = do
  renderCrosses origin b
  liftIO $ do
    RL.drawCube (addVectors origin $ Vector3 0.5    0 0) t 3 t RL.white
    RL.drawCube (addVectors origin $ Vector3 (-0.5) 0 0) t 3 t RL.white
    RL.drawCube (addVectors origin $ Vector3 0 0.5 0) 3 t t RL.white
    RL.drawCube (addVectors origin $ Vector3 0 (-0.5) 0) 3 t t RL.white
    pure $ i + 1
  where offset = fromIntegral (total - 1) * 0.5
        origin = Vector3 (CFloat (fromIntegral i - offset) * 4.5) 1.5 0
        t = 0.05


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


--------------------------------------------------------------------------------

addVectors :: Vector3 -> Vector3 -> Vector3
addVectors a b = Vector3
  (vector3'x a + vector3'x b)
  (vector3'y a + vector3'y b)
  (vector3'z a + vector3'z b)


multiplyVector :: Vector3 -> Float -> Vector3
multiplyVector a b = let b' = CFloat b in Vector3
  (vector3'x a * b')
  (vector3'y a * b')
  (vector3'z a * b')


multiplyVectors :: Vector3 -> Vector3 -> Vector3
multiplyVectors a b = Vector3
  (vector3'x a * vector3'x b)
  (vector3'y a * vector3'y b)
  (vector3'z a * vector3'z b)
