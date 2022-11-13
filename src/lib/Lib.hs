{-# OPTIONS -Wall #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Lib (main) where

import Control.Monad (unless)

import Apecs

import qualified Raylib as RL
import qualified Raylib.Colors as RL
import qualified Raylib.Constants as RL
import qualified Raylib.Types as RL
import Raylib.Types (Vector3 (..))

newtype Camera = Camera RL.Camera3D

makeWorldAndComponents "World" [''Camera]


main :: IO ()
main = initWorld >>= runSystem (initialise >> run >> terminate)


initialise :: System World ()
initialise = do
  let camera = RL.Camera3D (Vector3 0 1 0) (Vector3 2 1 1) (Vector3 0 1 0) 70 RL.cameraProjection'perspective
  set global $ Camera camera
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


render :: System World ()
render = do
  Camera camera <- get global
  liftIO $ do
    RL.beginDrawing
    RL.clearBackground RL.black
    RL.drawFPS 10 20
    RL.beginMode3D camera
    RL.drawGrid 10 1
    RL.drawCircle3D (Vector3 2 1 1) 2 (Vector3 0 1 0) 0 RL.white
    RL.drawLine3D (Vector3 3 0 1) (Vector3 1 2 1) RL.white
    RL.drawLine3D (Vector3 3 2 1) (Vector3 1 0 1) RL.white
    RL.drawCubeWiresV (Vector3 (-2) 1 0) (Vector3 1 2 1) RL.white
    RL.endMode3D
    RL.endDrawing


terminate :: System World ()
terminate = liftIO RL.closeWindow
