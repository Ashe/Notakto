{-# OPTIONS -Wall #-}

module Lib (main) where

import Control.Monad (unless)

import Apecs

import qualified Raylib as RL
import qualified Raylib.Constants as RL
import qualified Raylib.Types as RL
import Raylib.Types (Vector3 (..))

import Rendering
import Types

--------------------------------------------------------------------------------

main :: IO ()
main = initWorld >>= runSystem (initialise >> run >> terminate)


initialise :: System World ()
initialise = do
  let camera = RL.Camera3D (Vector3 0 1 6) (Vector3 0 1 0) (Vector3 0 1 0) 90
        RL.cameraProjection'perspective
      newBoard = Board Empty Empty Empty Empty Empty Empty Empty Empty Empty
  set global $ Camera camera
  newEntity_ $ Board
    Filled Empty Empty
    Empty Empty Empty
    Empty Empty Empty
  newEntity_ newBoard
  newEntity_ newBoard
  liftIO $ do
    RL.initWindow 1920 1080 "App"
    RL.setTargetFPS 60
    RL.setCameraMode camera RL.cameraMode'firstPerson


run :: System World ()
run = do
  update
  render
  shouldClose <- liftIO RL.windowShouldClose
  unless shouldClose run


update :: System World ()
update = do
  Camera c <- get global
  c' <- liftIO $ RL.updateCamera c
  set global $ Camera c'


terminate :: System World ()
terminate = liftIO RL.closeWindow
