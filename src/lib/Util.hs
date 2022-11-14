module Util (
  addVectors,
  multiplyVector,
  multiplyVectors,
) where

import Foreign.C.Types (CFloat(..))

import Raylib.Types (Vector3 (..))

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
