{-# OPTIONS -Wall #-}

module Lib (main) where

import Control.Monad (forM_, unless)
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
  target <- cfoldM (findLookAtTarget ray) NoTarget
  set global $ Aim ray target


findLookAtTarget :: RL.Ray -> LookAtTarget ->
                    (BoardComponent, PositionComponent, Entity) ->
                    System World LookAtTarget
findLookAtTarget ray target (_, Position p, e) = do
  if RL.rayCollision'hit hitInfo > 0 then
    getClosestTarget ray target $ Target e (findCell hitPos)
  else
    pure target
  where from = addVectors p $ Vector3 (-1.5) (-1.5) (-0.05)
        to = addVectors p $ Vector3 1.5 1.5 0.05
        hitInfo = RL.getRayCollisionBox ray $ RL.BoundingBox from to
        hitPos = subtractVectors (RL.rayCollision'point hitInfo) p


getClosestTarget :: RL.Ray -> LookAtTarget -> LookAtTarget ->
                    System World LookAtTarget
getClosestTarget ray a@(Target eA _) b@(Target eB _) = do
  Position posA <- get eA
  Position posB <- get eB
  let p = RL.ray'position ray
      distA = magnitudeVector $ subtractVectors posA p
      distB = magnitudeVector $ subtractVectors posB p
  pure $ if distA <= distB then a else b
getClosestTarget _ a NoTarget = pure a
getClosestTarget _ NoTarget b = pure b


findCell :: Vector3 -> Int
findCell (Vector3 x y _)
  | y > 0.5 = findCol 0 1 2
  | y < -0.5 = findCol 6 7 8
  | otherwise = findCol 3 4 5
  where findCol left center right
          | x < -0.5 = left
          | x > 0.5 = right
          | otherwise = center


updateCell :: BoardComponent -> Cell -> Int -> BoardComponent
updateCell b c 0 = b { _tl = c }
updateCell b c 1 = b { _tc = c }
updateCell b c 2 = b { _tr = c }
updateCell b c 3 = b { _ml = c }
updateCell b c 4 = b { _mc = c }
updateCell b c 5 = b { _mr = c }
updateCell b c 6 = b { _bl = c }
updateCell b c 7 = b { _bc = c }
updateCell b c 8 = b { _br = c }
updateCell b _ _ = b
