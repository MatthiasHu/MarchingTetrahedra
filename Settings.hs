module Settings (scalarField, numberOfVoxelsLinear, scalarFieldScale) where

import Graphics.UI.GLUT (GLfloat)

import qualified JuliaMandelbrot as JM


-- The number of voxels (sampling points) in any dimension.
-- Total number of voxels will therefore be numberOfVoxelsLinear ^ 3.
numberOfVoxelsLinear :: Int
numberOfVoxelsLinear = 20


-- Scale factor for the sampling points.
-- Put a lower (/higher) number here to see a smaller (/larger) excerpt of the field.
scalarFieldScale :: GLfloat
scalarFieldScale = 3


-- The actual scalar field to display the isosurface of.
-- Define your favourite scalar field below and specify it here.
scalarField :: GLfloat -> GLfloat -> GLfloat -> GLfloat
scalarField = scalarFieldHeart


-- several sample scalar fields:

scalarFieldHeart x0 y0 z0 = (x^2 + 9/4*y^2 + z^2 -1)^3 - x^2*z^3 - 9/80*y^2*z^3
  where z = y0
        x = -z0
        y = x0

scalarFieldTorus x y z = (y*y + sq (3 - sqrt (x*x+z*z)))
                         - sq 1
  where sq x = x*x

scalarFieldCompound x y z = scalarFieldTorus y x z + scalarField1 x y z

scalarField0 x y z = x*(y-5)

scalarField1 x y z = x*x-y*y*z*z

scalarField2 x y z = x*x*y*z' + x*x*z'*z' - y*y*y*z' - y*y*y
  where z' = z - 0

scalarField3 x y z = minimum [abs x, abs y, abs z] - 1

scalarField4 x y z = x*x+y*y*(y-4)+z*z*z*x+2

scalarField5 x y z = x*x*x+z*z+y*20


scalarFieldJM x y z =
  b ** fromIntegral tcrit - b ** fromIntegral t
  where
    t = JM.escapeTime tmax 0 z (x-0.5) y
    tmax :: Int
    tmax = 20
    tcrit = 15
    b = 0.5
