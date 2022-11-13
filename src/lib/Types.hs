{-# OPTIONS -Wall #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Types (
  World,
  initWorld,
  CameraComponent (..),
  BoardComponent (..),
  Cell (..),
) where

import Apecs

import qualified Raylib.Types as RL

--------------------------------------------------------------------------------

-----------
-- Types --
-----------

data Cell = Empty | Filled deriving (Show, Eq)

----------------
-- Components --
----------------

newtype CameraComponent = Camera RL.Camera3D


data BoardComponent = Board {
  _tl :: Cell, _tc :: Cell, _tr :: Cell,
  _ml :: Cell, _mc :: Cell, _mr :: Cell,
  _bl :: Cell, _bc :: Cell, _br :: Cell
} deriving (Show, Eq)


makeWorldAndComponents "World" [''CameraComponent, ''BoardComponent]
