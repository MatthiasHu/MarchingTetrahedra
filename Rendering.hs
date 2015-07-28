module Rendering (renderTriangle, renderTriangles, renderLines) where

import Graphics.UI.GLUT

import VectorArithmetic


type GVector = Vector3 GLfloat

type GTriangle = Vector3 (GVector, GVector)


renderTriangle :: GTriangle -> IO ()
renderTriangle (Vector3 (vert1,norm1) (vert2,norm2) (vert3,norm3)) = do
--  normal' normalVector -- (not normalized) face normal (for flat look)
  renderPrimitive Triangles $ do
    color $ (Color4 1.0 0.8 0.6 1.0 :: Color4 GLfloat)
    normal' norm1
    vertex' vert1
    normal' norm2
    vertex' vert2
    normal' norm3
    vertex' vert3
--  renderPrimitive LineLoop $ do
--    color $ (Color4 1.0 0.5 0.5 1.0 :: Color4 GLfloat)
--    vertex' vert1
--    vertex' vert2
--    vertex' vert3
  where normalVector = (vert2 #- vert1) #% (vert3 #- vert1)
        vertex' (Vector3 x y z) = vertex $ Vertex3 x y z
        normal' (Vector3 x y z) = normal $ Normal3 x y z


renderTriangles :: [GTriangle] -> IO ()
renderTriangles ts =
  mapM_ renderTriangle ts

renderLines :: [[Vertex3 GLfloat]] -> IO ()
renderLines lss = do
  color $ (Color4 0.3 0.6 1.0 1.0 :: Color4 GLfloat)
  mapM_ (\ls -> renderPrimitive LineStrip $ mapM_ vertex ls) lss
