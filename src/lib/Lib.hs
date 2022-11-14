{-# OPTIONS -Wall #-}

module Lib (main) where

import Control.Monad (forM_, unless, when)
import Foreign.C.Types (CFloat(..))

import Apecs

import qualified Raylib as RL
import qualified Raylib.Constants as RL
import qualified Raylib.Types as RL
import Raylib.Types (Vector3 (..))

import Rendering
import Types
import Util

--------------------------------------------------------------------------------

main :: IO ()
main = initWorld >>= runSystem (initialise >> run >> terminate)


initialise :: System World ()
initialise = do
  let camera = RL.Camera3D (Vector3 0 1 6) (Vector3 0 1 0) (Vector3 0 1 0) 90
        RL.cameraProjection'perspective
  set global $ Camera camera
  createBoards 3
  liftIO $ do
    RL.initWindow 1920 1080 "App"
    RL.setTargetFPS 60
    RL.setCameraMode camera RL.cameraMode'firstPerson


createBoards :: Int -> System World ()
createBoards n = do
  forM_ positions $ \p -> newEntity_ (newBoard, Position p)
  where newBoard = Board Empty Empty Empty Empty Empty Empty Empty Empty Empty
        positions = [Vector3 x' 1.5 0 | x <- [0..n - 1],
          let x' = (fromIntegral x - (fromIntegral (n - 1) / 2)) * 4.5]


terminate :: System World ()
terminate = liftIO RL.closeWindow


run :: System World ()
run = do
  update
  render
  shouldClose <- liftIO RL.windowShouldClose
  unless shouldClose run


update :: System World ()
update = do
  updateCamera
  handlePlayerAim
  markCurrentTarget


updateCamera :: System World ()
updateCamera = do
  Camera c <- get global
  c' <- liftIO $ RL.updateCamera c
  set global $ Camera c'


handlePlayerAim :: System World ()
handlePlayerAim = do
  windowWidth <- liftIO RL.getScreenWidth
  windowHeight <- liftIO RL.getScreenHeight
  Camera camera <- get global
  ray <- liftIO $ RL.getMouseRay (RL.Vector2
    (CFloat $ fromIntegral windowWidth / 2)
    (CFloat $ fromIntegral windowHeight / 2)) camera
  set global $ Aim ray


markCurrentTarget :: System World ()
markCurrentTarget = do
  aim@Aim{} <- get global
  cmapM $ checkBoard aim


checkBoard :: PlayerAimComponent -> (BoardComponent, PositionComponent) -> System World BoardComponent
checkBoard (Aim ray) (_, p) = do
  hit <- liftIO $ isHit ray p
  if hit then
    pure $ Board Filled Filled Filled Filled Filled Filled Filled Filled Filled
  else
    pure $ Board Empty Empty Empty Empty Empty Empty Empty Empty Empty


isHit :: RL.Ray -> PositionComponent -> IO Bool
isHit ray (Position p) = do
  let hitInfo = RL.getRayCollisionBox ray $ RL.BoundingBox from to
  -- liftIO $ print "Hello" -- <- uncomment to make this work?
  pure $ RL.raycollision'hit hitInfo > 0
  where from = addVectors p $ Vector3 (-1.5) (-1.5) (-0.05)
        to = addVectors p $ Vector3 1.5 1.5 0.05
