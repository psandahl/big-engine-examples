module Main where

import           BigE.Attribute.Vert_P (Vertex (..))
import           BigE.Math             (toRadians)
import           BigE.Mesh             (Mesh)
import qualified BigE.Mesh             as Mesh
import qualified BigE.Program          as Program
import           BigE.Runtime
import           BigE.Texture          (CubeMapFiles (..))
import qualified BigE.Texture          as Texture
import           BigE.Types
import           Control.Monad         (when)
import           Data.Bits             ((.|.))
import           Data.Vector.Storable  (Vector, fromList)
import           Graphics.GL           (GLfloat, GLuint)
import qualified Graphics.GL           as GL
import           Linear                (M44, V3 (..), V4 (..), axisAngle,
                                        lookAt, mkTransformation, normalize,
                                        perspective, zero, (!*), (!*!))

data State = State
    { program        :: !Program
    , mvpLoc         :: !Location
    , cubeTextureLoc :: !Location
    , mesh           :: !Mesh
    , cubeTexture    :: !Texture
    , persp          :: !(M44 GLfloat)
    , view           :: !(M44 GLfloat)
    , viewVector     :: !(V3 GLfloat)
    , turnLeft       :: !Bool
    , turnRight      :: !Bool
    , lookUp         :: !Bool
    , lookDown       :: !Bool
    } deriving Show

main :: IO ()
main = do
    let conf = Configuration { versionMajor = 3
                             , versionMinor = 3
                             , displayMode = SizedScreen (1024, 768)
                             , windowCaption = "Cube Map"
                             , setup = setupCallback
                             , animate = animateCallback
                             , render = renderCallback
                             , teardown = teardownCallback
                             }
    result <- runBigE conf
    print result

setupCallback :: Render State (Either String State)
setupCallback = do
    eProgram <- Program.fromFile
                    [ (VertexShader, "cube-map/vertex.glsl")
                    , (FragmentShader, "cube-map/fragment.glsl")
                    ]

    case eProgram of
        Right program' -> do

            let files = CubeMapFiles { negativeX = "cube-map/left.png"
                                     , positiveX = "cube-map/right.png"
                                     , negativeY = "cube-map/bottom.png"
                                     , positiveY = "cube-map/top.png"
                                     , negativeZ = "cube-map/back.png"
                                     , positiveZ = "cube-map/front.png"
                                     }
            eTexture <- Texture.fromFileCube files RGB8
            case eTexture of

                Right texture -> do

                    mvpLoc' <- Program.getUniformLocation program' "mvp"
                    cubeTextureLoc' <- Program.getUniformLocation program' "cube"
                    mesh' <- Mesh.fromVector StaticDraw (vertices 1) indices
                    (width, height) <- displayDimensions

                    setWindowSizeCallback $ Just windowSizeCallback
                    setKeyPressedCallback $ Just keyPressedCallback
                    setKeyReleasedCallback $ Just keyReleasedCallback

                    GL.glClearColor 1 1 1 0
                    GL.glEnable GL.GL_DEPTH_TEST

                    return $ Right State
                        { program = program'
                        , mvpLoc = mvpLoc'
                        , cubeTextureLoc = cubeTextureLoc'
                        , mesh = mesh'
                        , cubeTexture = texture
                        , persp = makePerspective width height
                        , view = lookAt cameraPos startView yAxis
                        , viewVector = startView
                        , turnLeft = False
                        , turnRight = False
                        , lookUp = False
                        , lookDown = False
                        }

                Left err -> return $ Left err

        Left err -> return $ Left err

animateCallback :: Render State ()
animateCallback = do
    duration <- realToFrac <$> frameDuration
    modifyAppState $ \state ->
        handleLeftTurn duration $
        handleRightTurn duration $
        handleLookUp duration $
        handleLookDown duration state

handleLeftTurn :: GLfloat -> State -> State
handleLeftTurn duration state
    | turnLeft state == True =
        let theta = (realToFrac duration) * 0.5 * pi
            viewVector' = rotateBy theta (viewVector state)
            view' = lookAt cameraPos (cameraPos + viewVector') yAxis
        in state { view = view', viewVector = viewVector' }
    | otherwise = state

handleRightTurn :: GLfloat -> State -> State
handleRightTurn duration state
    | turnRight state == True =
        let theta = (realToFrac duration) * 0.5 * (-pi)
            viewVector' = rotateBy theta (viewVector state)
            view' = lookAt cameraPos (cameraPos + viewVector') yAxis
        in state { view = view', viewVector = viewVector' }
    | otherwise = state

handleLookUp :: GLfloat -> State -> State
handleLookUp duration state
    | lookUp state == True =
        let V3 x y z = viewVector state
            y' = min 1.0 (y + duration)
            viewVector' = normalize $ V3 x y' z
            view' = lookAt cameraPos viewVector' yAxis
        in state { view = view', viewVector = viewVector' }
    | otherwise = state

handleLookDown :: GLfloat -> State -> State
handleLookDown duration state
    | lookDown state == True =
        let V3 x y z = viewVector state
            y' = max (-1.0) (y - duration)
            viewVector' = normalize $ V3 x y' z
            view' = lookAt cameraPos viewVector' yAxis
        in state { view = view', viewVector = viewVector' }
    | otherwise = state

renderCallback :: Render State ()
renderCallback = do
    GL.glClear (GL.GL_COLOR_BUFFER_BIT .|. GL.GL_DEPTH_BUFFER_BIT)

    state <- getAppStateUnsafe

    Program.enable (program state)
    Mesh.enable (mesh state)

    let mvp = persp state !*! view state
    setUniform (mvpLoc state) mvp

    Mesh.render Triangles (mesh state)

    Mesh.disable
    Program.disable

teardownCallback :: Render State ()
teardownCallback = do
    state <- getAppStateUnsafe
    Mesh.delete (mesh state)
    Program.delete (program state)

windowSizeCallback :: Int -> Int -> Render State ()
windowSizeCallback width height =
    modifyAppState (\state -> state { persp = makePerspective width height })

keyPressedCallback :: Key -> ModifierKeys -> Render State ()
keyPressedCallback key _ = do
    when (key == Key'Left) $
        modifyAppState $ \state -> state { turnLeft = True }
    when (key == Key'Right) $
        modifyAppState $ \state -> state { turnRight = True }
    when (key == Key'Up) $
        modifyAppState $ \state -> state { lookUp = True }
    when (key == Key'Down) $
        modifyAppState $ \state -> state { lookDown = True }

keyReleasedCallback :: Key -> ModifierKeys -> Render State ()
keyReleasedCallback key _ = do
    when (key == Key'Left) $
        modifyAppState $ \state -> state { turnLeft = False }
    when (key == Key'Right) $
        modifyAppState $ \state -> state { turnRight = False }
    when (key == Key'Up) $
        modifyAppState $ \state -> state { lookUp = False }
    when (key == Key'Down) $
        modifyAppState $ \state -> state { lookDown = False }

vertices :: GLfloat -> Vector Vertex
vertices width =
    fromList
        [ Vertex { position = V3   width    width  (-width) }
        , Vertex { position = V3 (-width)   width  (-width) }
        , Vertex { position = V3 (-width) (-width) (-width) }
        , Vertex { position = V3   width  (-width) (-width) }
        , Vertex { position = V3   width    width    width }
        , Vertex { position = V3 (-width)   width    width }
        , Vertex { position = V3 (-width) (-width)   width }
        , Vertex { position = V3   width  (-width)   width }
        ]

indices :: Vector GLuint
indices =
    fromList
        [ -- Negative z
          0, 1, 2, 0, 2, 3
          -- Positive z
        , 5, 4, 7, 5, 7, 6
          -- Negative x
        , 1, 5, 6, 1, 6, 2
          -- Positive x
        , 4, 0, 3, 4, 3, 7
          -- Negative y
        , 3, 2, 6, 3, 6, 7
          -- Positive y
        , 4, 5, 1, 4, 1, 0
        ]

makePerspective :: Int -> Int -> M44 GLfloat
makePerspective width height =
    perspective (toRadians 45) (fromIntegral width / fromIntegral height) 0.001 1000

cameraPos :: V3 GLfloat
cameraPos = V3 0 0 0

startView :: V3 GLfloat
startView = V3 0 0 (-1)

rotateBy :: GLfloat -> V3 GLfloat -> V3 GLfloat
rotateBy theta (V3 x y z) =
    let rotation = makeRotation theta
        V4 x' y' z' _w = rotation !* V4 x y z 0
    in normalize $ V3 x' y' z'

makeRotation :: GLfloat -> M44 GLfloat
makeRotation theta = mkTransformation (axisAngle yAxis theta) zero

yAxis :: V3 GLfloat
yAxis = V3 0 1 0
