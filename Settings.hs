module Settings
  ( scalarFields
  , curves
  , numberOfVoxelsLinear
  , curveStep
  , scale
  ) where

import Graphics.UI.GLUT (GLfloat, Vertex3(..))


-- The number of voxels (sampling points) in any dimension.
-- Total number of voxels will therefore be numberOfVoxelsLinear ^ 3.
numberOfVoxelsLinear :: Int
numberOfVoxelsLinear = 50

-- The parameter step size for rendering the curve(s).
curveStep :: GLfloat
curveStep = 0.05

-- Scale factor for the sampling points.
-- Put a lower (/higher) number here to see a smaller (/larger)
-- excerpt of the surface(s) (and curve(s)).
scale :: GLfloat
scale = 5

-- Curve(s) to display.
curves :: [GLfloat -> Vertex3 GLfloat]
curves =
 [ \t -> Vertex3
     ( t )
     ( t*t )
     ( t*t*t )
 ]

-- The actual scalar fields to display the isosurfaces of.
-- Singleton list for only one surface.
-- Define your favourite scalar field below and specify it here.
scalarFields :: [GLfloat -> GLfloat -> GLfloat -> GLfloat]
scalarFields = [scalarField6_1, scalarField6_2]


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


scalarField6_1 x y z = x*x - y

scalarField6_2 x y z = x*x*x - z
