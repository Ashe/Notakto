module Util (
  destroyEntity,
) where

import Apecs

import Raylib.Types (Vector3 (..))

import Types

--------------------------------------------------------------------------------


destroyEntity :: Entity -> System World ()
destroyEntity e = destroy e (Proxy :: Proxy AllComponents)
