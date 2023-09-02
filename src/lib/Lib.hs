{-# OPTIONS -Wall #-}

module Lib (main) where

import Control.Applicative (liftA2)
import Control.Monad (forM_, unless, when)

import Apecs

import qualified Raylib.Core as RL
import qualified Raylib.Core.Models as RL
import qualified Raylib.Types as RL
import qualified Raylib.Util as RL
import qualified Raylib.Util.Camera as RL
import Raylib.Types (Vector2 (..), Vector3 (..))
import Raylib.Util.Math

import Rendering
import Types
import Util

--------------------------------------------------------------------------------

main :: IO ()
main = initWorld >>= runSystem (do
  window <- initialise
  run
  terminate window)


initialise :: System World RL.WindowResources
initialise = do
  let camera = RL.Camera3D (Vector3 0 1 6) (Vector3 0 1 0) (Vector3 0 1 0) 90
        RL.CameraPerspective
  set global (Camera camera, Red)
  newGame
  liftIO $ do
    window <- RL.initWindow 1920 1080 "App"
    RL.setTargetFPS 60
    RL.disableCursor
    pure window


newGame :: System World ()
newGame = do
  cmapM_ deleteBoard
  createBoards 3
  where deleteBoard (Board{}, e) = destroyEntity e


createBoards :: Int -> System World ()
createBoards n = do
  forM_ positions $ \p -> newEntity_ (newBoard, Position p)
  where newBoard = Board Empty Empty Empty Empty Empty Empty Empty Empty Empty
        positions = [Vector3 x' 1.5 0 | x <- [0..n - 1],
          let x' = (fromIntegral x - (fromIntegral (n - 1) / 2)) * 4.5]


terminate :: RL.WindowResources -> System World ()
terminate window = liftIO $ RL.closeWindow window


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
  clicked <- liftIO $ RL.isMouseButtonPressed RL.MouseButtonLeft
  when clicked $ do
    handleLeftClick


updateCamera :: System World ()
updateCamera = do
  Camera c <- get global
  newCam <- liftIO $ do
    dt <- RL.getFrameTime
    forward <- checkKey RL.KeyW RL.KeyUp
    left <- checkKey RL.KeyA RL.KeyLeft
    backward <- checkKey RL.KeyS RL.KeyDown
    right <- checkKey RL.KeyD RL.KeyRight
    Vector2 i j <- RL.getMouseDelta
    let speed = 5.0
        turnspeed = 1
        Vector3 x _ z =
          (RL.getCameraForward c |* (forward - backward)) |+|
          (RL.getCameraRight c |* (right - left))
        c' = RL.cameraMove c $ safeNormalize (Vector3 x 0 z) |* (speed * dt)
        c'' = RL.cameraYaw c' (-i * turnspeed * dt) False
    pure $ RL.cameraPitch c'' (-j * turnspeed * dt) False False False
  set global $ Camera newCam
  where checkKey a b =
          liftA2 (\x y -> if x || y then 1 else 0) (RL.isKeyDown a) (RL.isKeyDown b)
        safeNormalize v
          | magnitude v == 0 = v
          | otherwise = vectorNormalize v


handlePlayerAim :: System World ()
handlePlayerAim = do
  windowWidth <- liftIO RL.getScreenWidth
  windowHeight <- liftIO RL.getScreenHeight
  Camera camera <- get global
  ray <- liftIO $ RL.getMouseRay (RL.Vector2
    (fromIntegral windowWidth / 2)
    (fromIntegral windowHeight / 2)) camera
  target <- cfoldM (findLookAtTarget ray) NoTarget
  set global $ Aim ray target


findLookAtTarget :: RL.Ray -> LookAtTarget -> (BoardComponent,
                    PositionComponent, Not DeathComponent, Entity) ->
                    System World LookAtTarget
findLookAtTarget ray target (_, Position p, _, e) = do
  if RL.rayCollision'hit hitInfo then
    getClosestTarget ray target $ Target e (findCell hitPos)
  else
    pure target
  where from = p |+| Vector3 (-1.5) (-1.5) (-0.05)
        to = p |+| Vector3 1.5 1.5 0.05
        hitInfo = RL.getRayCollisionBox ray $ RL.BoundingBox from to
        hitPos = RL.rayCollision'point hitInfo |-| p


getClosestTarget :: RL.Ray -> LookAtTarget -> LookAtTarget ->
                    System World LookAtTarget
getClosestTarget ray a@(Target eA _) b@(Target eB _) = do
  Position posA <- get eA
  Position posB <- get eB
  let p = RL.ray'position ray
      distA = magnitude $ posA |-| p
      distB = magnitude $ posB |-| p
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


handleLeftClick :: System World ()
handleLeftClick = do
  player <- get global
  needsRestart <- checkForGameOver
  if not needsRestart then do
    moveMade <- tryPlaceCross player
    when moveMade $ do
      cmap tryKillBoard
      isGameOver <- checkForGameOver
      if isGameOver then
        liftIO $ putStrLn $ "Game over! " ++ show player ++ " loses!"
      else do
        let nextPlayer = if player == Red then Blue else Red
        set global nextPlayer
        liftIO $ putStrLn $ "It's " ++ show nextPlayer ++ "'s turn!"
  else do
    newGame
    liftIO $ putStrLn $ "Restarted game! It's " ++ show player ++ "'s turn!"


tryPlaceCross :: PlayerComponent -> System World Bool
tryPlaceCross player = do
  Aim _ target <- get global
  case target of
    NoTarget -> pure False
    Target e i -> do
      board <- get e
      if getCell board i == Empty then do
        set e $ setCell board i $ Filled player
        pure True
      else
        pure False


checkForGameOver :: System World Bool
checkForGameOver = do
  let countAlive :: Int -> (BoardComponent, Not DeathComponent) -> Int
      countAlive c (_, _) = c + 1
  count <- cfold countAlive 0
  pure $ count <= 0


tryKillBoard :: (BoardComponent, Not DeathComponent) -> Maybe DeathComponent
tryKillBoard (bc, _) = if check cellCombos then Just Dead else Nothing
  where check = foldl (\dead (a, b, c) -> dead || checkCombo a b c) False
        checkCombo a b c = checkCell a && checkCell b && checkCell c
        checkCell c = case getCell bc c of Filled _ -> True ; _ -> False


cellCombos :: [(Int, Int, Int)]
cellCombos = [
  -- Horizontal
  (0, 1, 2),
  (3, 4, 5),
  (6, 7, 8),
  -- Vertical
  (0, 3, 6),
  (1, 4, 7),
  (2, 5, 8),
  -- Diagonal
  (0, 4, 8),
  (2, 4, 6)
  ]


getCell :: BoardComponent -> Int -> Cell
getCell b 0 =_tl b
getCell b 1 =_tc b
getCell b 2 =_tr b
getCell b 3 =_ml b
getCell b 4 =_mc b
getCell b 5 =_mr b
getCell b 6 =_bl b
getCell b 7 =_bc b
getCell b 8 =_br b
getCell _ _ = Empty


setCell :: BoardComponent -> Int -> Cell -> BoardComponent
setCell b 0 c = b { _tl = c }
setCell b 1 c = b { _tc = c }
setCell b 2 c = b { _tr = c }
setCell b 3 c = b { _ml = c }
setCell b 4 c = b { _mc = c }
setCell b 5 c = b { _mr = c }
setCell b 6 c = b { _bl = c }
setCell b 7 c = b { _bc = c }
setCell b 8 c = b { _br = c }
setCell b _ _ = b
