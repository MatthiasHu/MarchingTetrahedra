module TrianglesLookup (trianglesLookup) where

import Graphics.UI.GLUT

import UnsortedPair


-- All triangles oriented towards the "True" tetrahedron vertices.
trianglesLookup :: Ord a => Vector4 a -> Vector4 Bool -> [Vector3 (UnsortedPair a)]
trianglesLookup v1 (Vector4 False b2 b3 b4) =
  map oppositeDirection $
    trianglesLookup v1 (Vector4 True (not b2) (not b3) (not b4))
trianglesLookup (Vector4 a b c d) (Vector4 True  False False False) =
  [triangle  a b  a d  a c]
trianglesLookup (Vector4 a b c d) (Vector4 True  True  False False) =
  [triangle  a d  a c  b d,  triangle  a c  b c  b d]
trianglesLookup (Vector4 a b c d) (Vector4 True  False True  False) =
  [triangle  a b  a d  c d,  triangle  a b  c d  b c]
trianglesLookup (Vector4 a b c d) (Vector4 True  True  True  False) =
  [triangle  a d  c d  b d]
trianglesLookup (Vector4 a b c d) (Vector4 True  False False True ) =
  [triangle  a b  c d   a c,  triangle  a b  b d  c d]
trianglesLookup (Vector4 a b c d) (Vector4 True  True  False True ) =
  [triangle  a c  b c  c d]
trianglesLookup (Vector4 a b c d) (Vector4 True  False True  True ) =
  [triangle  a b  b d  b c]
trianglesLookup (Vector4 a b c d) (Vector4 True  True  True  True ) =
  []
trianglesLookup _ _ = []


triangle  a b  c d  e f  = Vector3 (upair a b) (upair c d) (upair e f)


oppositeDirection :: Vector3 a -> Vector3 a
oppositeDirection (Vector3 a b c) = Vector3 b a c
