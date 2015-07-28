module Tetrahedra
  ( dissectCube
  , interpolateVertices
  , doTetrahedron
  , VoxelIdentifier
  , GTriangle
  ) where


import Graphics.UI.GLUT

import UnsortedPair
import TrianglesLookup
import VectorArithmetic



type FaceVertexIdentifier = UnsortedPair VoxelIdentifier

type VoxelIdentifier = Vector3 Int

type VoxelValue = GLfloat

type AbstractTriangle = Vector3 FaceVertexIdentifier

type GVector = Vector3 GLfloat

type GTriangle = Vector3 (GVector, GVector) -- (vertex, normal)

type VoxelData = VoxelIdentifier -> VoxelValue

type Tetrahedron = Vector4 VoxelIdentifier



interpolateVertices :: VoxelData -> AbstractTriangle -> GTriangle
interpolateVertices voxelData = fmap (interpolateVertex voxelData)


interpolateVertex :: VoxelData -> FaceVertexIdentifier -> (GVector, GVector)
interpolateVertex voxelData fvi = ((root' #* voxf1) #+ (root #* voxf2)
                                  ,(root' #* grad1) #+ (root #* grad2))
  where (vox1, vox2) = pair fvi
        voxf1 = fmap fromIntegral vox1
        voxf2 = fmap fromIntegral vox2
        grad1 = gradient voxelData vox1
        grad2 = gradient voxelData vox2
        root = (voxelData vox1)/(voxelData vox1 - voxelData vox2)
        root' = 1-root


gradient :: VoxelData -> VoxelIdentifier -> GVector
gradient voxelData voxel = Vector3
  (voxelData (voxel #- Vector3 1 0 0) - voxelData (voxel #+ Vector3 1 0 0))
  (voxelData (voxel #- Vector3 0 1 0) - voxelData (voxel #+ Vector3 0 1 0))
  (voxelData (voxel #- Vector3 0 0 1) - voxelData (voxel #+ Vector3 0 0 1))


doTetrahedron :: VoxelData -> Tetrahedron -> [AbstractTriangle]
doTetrahedron voxelData tetrahedron =
  trianglesLookup tetrahedron (fmap ((>0) . voxelData) tetrahedron)



dissectCube = dissectCube5


-- dissecting a cube into six tetrahedra (no mirroring needed)
dissectCube6 :: VoxelIdentifier -> [Tetrahedron]
dissectCube6 v0 =
  [Vector4 v0  (v0 #+ dx #+ dy #+ dz)   (v0 #+ dx)       (v0 #+ dx #+ dz)
  ,Vector4 v0  (v0 #+ dx #+ dy #+ dz)   (v0 #+ dz)       (v0 #+ dx #+ dz)
  ,Vector4 v0  (v0 #+ dx #+ dy #+ dz)   (v0 #+ dz)       (v0 #+ dy #+ dz)
  ,Vector4 v0  (v0 #+ dx #+ dy #+ dz)   (v0 #+ dy)       (v0 #+ dy #+ dz)
  ,Vector4 v0  (v0 #+ dx #+ dy #+ dz)   (v0 #+ dy)       (v0 #+ dx #+ dy)
  ,Vector4 v0  (v0 #+ dx #+ dy #+ dz)   (v0 #+ dx)       (v0 #+ dx #+ dy)
  ]
  where dx = Vector3 1 0 0
        dy = Vector3 0 1 0
        dz = Vector3 0 0 1



-- Dissecting a cube into five tetrahedra, mirroring as neccessary
-- to keep the triangle edges of this and any adjacent cube coincident.
-- All tetrahedra with the same (positive) orientation.
dissectCube5 :: VoxelIdentifier -> [Tetrahedron]
dissectCube5 (Vector3 x y z) =
  map rectifyOrientation
  [Vector4 (v0)             (v0 #+ dy)       (v0 #+ dx)       (v0 #+ dz)
  ,Vector4 (v0 #+ dy #+ dz) (v0 #+ dy)       (v0 #+ dz)       (v0 #+ dy #+ dz #+ dx)
  ,Vector4 (v0 #+ dy)       (v0 #+ dy #+ dx) (v0 #+ dx)       (v0 #+ dy #+ dz #+ dx)
  ,Vector4 (v0 #+ dz)       (v0 #+ dx)       (v0 #+ dz #+ dx) (v0 #+ dy #+ dz #+ dx)
  ,Vector4 (v0 #+ dy)       (v0 #+ dx)       (v0 #+ dz)       (v0 #+ dy #+ dz #+ dx)
  ]
  where (x0, dx') = if x `mod` 2 == 0 then (x, 1) else (x+1, -1)
        (y0, dy') = if y `mod` 2 == 0 then (y, 1) else (y+1, -1)
        (z0, dz') = if z `mod` 2 == 0 then (z, 1) else (z+1, -1)
        v0 = Vector3 x0 y0 z0
        dx = Vector3 dx' 0 0
        dy = Vector3 0 dy' 0
        dz = Vector3 0 0 dz'
        rectifyOrientation = if (x+y+z) `mod` 2 == 0 then id else oppositeOrientation
        oppositeOrientation (Vector4 a b c d) = Vector4 b a c d

