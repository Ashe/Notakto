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

  clicked <- liftIO $ RL.isMouseButtonPressed 0
  when clicked $ do
    handleLeftClick


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


findLookAtTarget :: RL.Ray -> LookAtTarget -> (BoardComponent,
                    PositionComponent, Not DeathComponent, Entity) ->
                    System World LookAtTarget
findLookAtTarget ray target (_, Position p, _, e) = do
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


handleLeftClick :: System World ()
handleLeftClick = do
  moveMade <- tryPlaceCross
  when moveMade $ do
    isGameOver <- checkForGameOver
    if isGameOver then
      liftIO $ putStrLn "Game over!"
    else
      liftIO $ putStrLn "Next turn!"


tryPlaceCross :: System World Bool
tryPlaceCross = do
  Aim _ target <- get global
  case target of
    NoTarget -> pure False
    Target e i -> do
      board <- get e
      if getCell board i == Empty then do
        set e $ setCell board i Filled
        pure True
      else
        pure False


checkForGameOver :: System World Bool
checkForGameOver = do
  cmap tryKillBoard
  let countAlive :: Int -> (BoardComponent, Not DeathComponent) -> Int
      countAlive c (_, _) = c + 1
  count <- cfold countAlive 0
  pure $ count <= 0


tryKillBoard :: (BoardComponent, Not DeathComponent) -> Maybe DeathComponent
tryKillBoard (bc, _) = if check cellCombos then Just Dead else Nothing
  where check = foldl (\dead (a, b, c) -> dead || checkCombo a b c) False
        checkCombo a b c = checkCell a && checkCell b && checkCell c
        checkCell c = getCell bc c == Filled


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
