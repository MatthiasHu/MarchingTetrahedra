
import Data.Array
import Graphics.UI.GLUT
import Data.IORef
import Data.Graph.Inductive.Query.Monad ((><))

import Tetrahedra
import Rendering
import VectorArithmetic
import Settings as Settings


main :: IO ()
main = do
  (progName, args) <- getArgsAndInitialize
  initialDisplayMode $= [WithDepthBuffer]
  initialWindowSize $= Size 650 650
  window <- createWindow "marching tetrahedra"
  -- setup state reference
  stateRef <- newIORef state0
  -- setup general gl stuff
  matrixMode $= Projection
  frustum (-0.05) (0.05) (-0.05) (0.05) (0.1) (100.0)
  matrixMode $= Modelview 0
  clearColor $= Color4 0.0 0.0 0.0 1.0
  depthFunc $= Just Lequal
  -- nomalize and flip normal vectors
  normalize $= Enabled
  lightModelTwoSide $= Enabled
  -- setup lighting
  lighting $= Enabled
  lightModelLocalViewer $= Enabled
  materialEmission FrontAndBack $= Color4 0.0 0.0 0.0 1.0
  materialSpecular FrontAndBack $= Color4 1.0 1.0 1.0 1.0
  materialShininess FrontAndBack $= 30
  colorMaterial $= Just (FrontAndBack, AmbientAndDiffuse)
  -- set callbacks
  reshapeCallback $= Just reshape
  displayCallback $= display stateRef
  keyboardMouseCallback $= Just (keyboardMouse stateRef)
  -- start main loop
  mainLoop



reshape :: ReshapeCallback
reshape size = do
  viewport $= (Position 0 0, size)


display :: IORef State -> DisplayCallback
display stateRef = do
  state <- get stateRef
  -- pre-rendering stuff:
  matrixMode $= Modelview 0
  loadIdentity
  clear [ColorBuffer, DepthBuffer, StencilBuffer]
  -- one light
  light (Light 0) $= Enabled
  position (Light 0) $= Vertex4 0 0 0 1
  ambient (Light 0) $= Color4 0.1 0.1 0.1 1
  diffuse (Light 0) $= Color4 0.6 0.8 1.0 1
  specular (Light 0) $= Color4 0.3 0.4 0.5 1
  attenuation (Light 0) $= (0, 0.1, 0.02)
  -- get in place
  translate $ (Vector3 0 0 (-15) :: Vector3 GLfloat)
  rotate ((inclination state)*180/pi) $ Vector3 (1::GLfloat) 0 0
  rotate ((rotation state)*180/pi) $ Vector3 0 (1::GLfloat) 0
  -- another light
  light (Light 1) $= Enabled
  position (Light 1) $= Vertex4 0 8 0 1
  ambient (Light 1) $= Color4 0.1 0.1 0.1 1
  diffuse (Light 1) $= Color4 1.0 0.8 0.5 1
  specular (Light 1) $= Color4 0.5 0.4 0.2 1
  attenuation (Light 1) $= (0, 0.2, 0.02)
  -- scene
  let scaleFactor = 10/(fromIntegral Settings.numberOfVoxelsLinear) :: GLfloat in
    scale scaleFactor scaleFactor scaleFactor
  newList <- callOrDefineList (displayList state) (renderTriangles (gTriangles state))
  stateRef $= state {displayList = Just newList}
  flush



callOrDefineList :: Maybe DisplayList -> IO () -> IO (DisplayList)
callOrDefineList (Just list) _ = callList list >> return list
callOrDefineList Nothing renderingStuff = defineNewList CompileAndExecute renderingStuff


voxelsToTriangles :: Array VoxelIdentifier GLfloat -> [GTriangle]
voxelsToTriangles voxels = map (interpolateVertices (voxels !)) allAbstractTriangles
  where allCubes = range $ ((#+ Vector3 1 1 1) >< (#- Vector3 2 2 2)) $ bounds voxels
        allTetrahedra = concatMap dissectCube allCubes
        allAbstractTriangles = concatMap (doTetrahedron (voxels !)) allTetrahedra


keyboardMouse :: IORef State -> KeyboardMouseCallback
keyboardMouse stateRef (SpecialKey KeyLeft) Down _ _ = changeState stateRef $
  \state -> state {rotation = rotation state - rotatespeed}
keyboardMouse stateRef (SpecialKey KeyRight) Down _ _ = changeState stateRef $
  \state -> state {rotation = rotation state + rotatespeed}
keyboardMouse stateRef (SpecialKey KeyUp) Down _ _ = changeState stateRef $
  \state -> state {inclination = inclination state - rotatespeed}
keyboardMouse stateRef (SpecialKey KeyDown) Down _ _ = changeState stateRef $
  \state -> state {inclination = inclination state + rotatespeed}
keyboardMouse stateRef _ _ _ _ = return ()


movespeed = 0.1 :: GLfloat
rotatespeed = 0.1 ::GLfloat



changeState :: (IORef State) -> (State -> State) -> IO ()
changeState stateRef f = do
  state <- get stateRef
  stateRef $= f state
  postRedisplay Nothing



data State = State
  {rotation :: GLfloat
  ,inclination :: GLfloat  -- rotation and inclination in rad
  ,gTriangles :: [GTriangle]
  ,displayList :: Maybe DisplayList
  }

state0 = State {rotation=0, inclination=pi/6, gTriangles=voxelsToTriangles voxels, displayList = Nothing}

voxels :: Array VoxelIdentifier GLfloat
voxels = array (Vector3 (-r) (-r) (-r), Vector3 r r r)
  [(Vector3 x y z, Settings.scalarField (scale'*fromIntegral x) (scale'*fromIntegral y) (scale'*fromIntegral z))
  | x <- [-r..r], y <- [-r..r], z <- [-r..r]]
  where r = Settings.numberOfVoxelsLinear `div` 2
        scale' = Settings.scalarFieldScale / (fromIntegral Settings.numberOfVoxelsLinear)

