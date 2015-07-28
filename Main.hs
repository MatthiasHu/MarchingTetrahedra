
import Data.Array
import Graphics.UI.GLUT
import Data.IORef

import Tetrahedra
import Rendering
import VectorArithmetic
import qualified Settings


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
  clearColor $= Color4 1.0 1.0 1.0 1.0
  lineWidth $= 3.0
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
reshape (Size w h) = do
  viewport $= (Position x0 y0, Size dim dim)
  where dim = min w h
        x0 = (w-dim) `div` 2
        y0 = (h-dim) `div` 2


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
  --diffuse (Light 0) $= Color4 0.6 0.8 1.0 1
  diffuse (Light 0) $= Color4 0.8 0.8 0.8 1
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
  newList <- callOrDefineList (displayList state) (renderScene state)
  stateRef $= state {displayList = Just newList}
  flush


renderScene :: State -> IO ()
renderScene state = do
  -- surface(s)
  depthFunc $= Just Lequal
  preservingMatrix $ do
    scale' $ 5/(fromIntegral (Settings.numberOfVoxelsLinear `div` 2))
    renderTriangles (gTriangles state) 
  -- curve(s)
  depthFunc $= Just Always
  preservingMatrix $ do
    scale' $ 5/Settings.scale
    renderLines (gLines state)
  where scale' :: GLfloat -> IO ()
        scale' s = scale s s s



callOrDefineList :: Maybe DisplayList -> IO () -> IO (DisplayList)
callOrDefineList (Just list) _ = callList list >> return list
callOrDefineList Nothing renderingStuff = defineNewList CompileAndExecute renderingStuff


voxelsToTriangles :: Array VoxelIdentifier GLfloat -> [GTriangle]
voxelsToTriangles voxels = map (interpolateVertices (voxels !)) allAbstractTriangles
  where allCubes = range $ ((#+ Vector3 1 1 1) >< (#- Vector3 2 2 2)) $ bounds voxels
        allTetrahedra = concatMap dissectCube allCubes
        allAbstractTriangles = concatMap (doTetrahedron (voxels !)) allTetrahedra
        f >< g = \(x, y) -> (f x, g y)

curveToLines :: (GLfloat -> Vertex3 GLfloat) -> [Vertex3 GLfloat]
curveToLines curve =
  reverse [curve (-i*step) | i<-[0..1000], maxNorm (curve (-i*step)) <= bound]
  ++      [curve ( i*step) | i<-[0..1000], maxNorm (curve ( i*step)) <= bound]
  where
    step = Settings.curveStep
    maxNorm (Vertex3 x y z) = maximum $ map abs [x, y, z]
    bound = Settings.scale


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
  { rotation :: GLfloat
  , inclination :: GLfloat  -- rotation and inclination in rad
  , gTriangles :: [GTriangle]
  , gLines :: [[Vertex3 GLfloat]]
  , displayList :: Maybe DisplayList
  }

state0 = State
  { rotation = 0
  , inclination = pi/6
  , gTriangles = concatMap (voxelsToTriangles . voxels) Settings.scalarFields
  , gLines = map curveToLines Settings.curves
  , displayList = Nothing
  }

voxels :: (GLfloat -> GLfloat -> GLfloat -> GLfloat) -> Array VoxelIdentifier GLfloat
voxels scalarField = array (Vector3 (-r) (-r) (-r), Vector3 r r r)
  [ ( Vector3 x y z
    , scalarField (scale'*fromIntegral x)
                  (scale'*fromIntegral y)
                  (scale'*fromIntegral z)
    )
  | x <- [-r..r], y <- [-r..r], z <- [-r..r] ]
  where r = Settings.numberOfVoxelsLinear `div` 2
        scale' = Settings.scale /
                   (fromIntegral (Settings.numberOfVoxelsLinear `div` 2))

