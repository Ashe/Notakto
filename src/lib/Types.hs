{-# OPTIONS -Wall #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Types (
  World,
  initWorld,
  Cell (..),
  AllComponents,
  LookAtTarget (..),
  PositionComponent (..),
  CameraComponent (..),
  BoardComponent (..),
  DeathComponent (..),
  PlayerComponent (..),
  PlayerAimComponent (..),
) where

import Apecs

import qualified Raylib.Types as RL

--------------------------------------------------------------------------------

-----------
-- Types --
-----------

data Cell = Empty | Filled PlayerComponent deriving (Show, Eq)


data LookAtTarget = NoTarget | Target Entity Int deriving (Show, Eq)

----------------
-- Components --
----------------

newtype PositionComponent = Position RL.Vector3 deriving (Show, Eq)


newtype CameraComponent = Camera RL.Camera3D


data BoardComponent = Board {
  _tl :: Cell, _tc :: Cell, _tr :: Cell,
  _ml :: Cell, _mc :: Cell, _mr :: Cell,
  _bl :: Cell, _bc :: Cell, _br :: Cell
} deriving (Show, Eq)


data DeathComponent = Dead deriving (Show, Eq)


data PlayerComponent = Red | Blue deriving (Show, Eq)


data PlayerAimComponent = Aim RL.Ray LookAtTarget deriving (Show, Eq)


makeWorldAndComponents "World" [
  ''PositionComponent,
  ''CameraComponent,
  ''BoardComponent,
  ''DeathComponent,
  ''PlayerComponent,
  ''PlayerAimComponent
  ]

type AllComponents = (PositionComponent, CameraComponent, BoardComponent,
  DeathComponent, PlayerComponent, PlayerAimComponent)
