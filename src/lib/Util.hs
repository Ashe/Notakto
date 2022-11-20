module Util (
  destroyEntity,
  addVectors,
  subtractVectors,
  multiplyVector,
  multiplyVectors,
  magnitudeVector
) where

import Apecs

import Foreign.C.Types (CFloat(..))

import Raylib.Types (Vector3 (..))

import Types

--------------------------------------------------------------------------------


destroyEntity :: Entity -> System World ()
destroyEntity e = destroy e (Proxy :: Proxy AllComponents)


addVectors :: Vector3 -> Vector3 -> Vector3
addVectors a b = Vector3
  (vector3'x a + vector3'x b)
  (vector3'y a + vector3'y b)
  (vector3'z a + vector3'z b)


subtractVectors :: Vector3 -> Vector3 -> Vector3
subtractVectors a b = Vector3
  (vector3'x a - vector3'x b)
  (vector3'y a - vector3'y b)
  (vector3'z a - vector3'z b)


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


magnitudeVector :: Vector3 -> Float
magnitudeVector (Vector3 x y z) =
  let CFloat f = sqrt $ (x * x) + (y * y) + (z * z) in f
