module VectorArithmetic ((#+), (#-), (#*), (#%)) where

import Graphics.UI.GLUT


(#+) :: Num a => Vector3 a -> Vector3 a -> Vector3 a
Vector3 x1 y1 z1 #+ Vector3 x2 y2 z2 = Vector3 (x1+x2) (y1+y2) (z1+z2)

(#-) :: Num a => Vector3 a -> Vector3 a -> Vector3 a
Vector3 x1 y1 z1 #- Vector3 x2 y2 z2 = Vector3 (x1-x2) (y1-y2) (z1-z2)

(#*) :: Num a => a -> Vector3 a -> Vector3 a
s #* Vector3 x y z = Vector3 (s*x) (s*y) (s*z)

-- cross product
(#%) :: Num a => Vector3 a -> Vector3 a -> Vector3 a
Vector3 x1 y1 z1 #% Vector3 x2 y2 z2 = Vector3 (y1*z2-z1*y2) (z1*x2-x1*z2) (x1*y2-y1*x2)
