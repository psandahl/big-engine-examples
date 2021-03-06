module Main where

import           BigE.Attribute.Vert_P_C (Vertex (..))
import           BigE.Math               (toRadians)
import           BigE.Mesh               (Mesh)
import qualified BigE.Mesh               as Mesh
import qualified BigE.Program            as Program
import           BigE.Runtime
import           BigE.Types
import           Data.Vector.Storable    (Vector, fromList)
import           Graphics.GL             (GLfloat, GLuint)
import qualified Graphics.GL             as GL
import           Linear                  (M44, V3 (..), V4 (..), axisAngle,
                                          lookAt, mkTransformation, perspective,
                                          zero, (!*!))

data State = State
    { program  :: !Program
    , mvpLoc   :: !Location
    , mesh     :: !Mesh
    , rotation :: !GLfloat
    , persp    :: !(M44 GLfloat)
    , view     :: !(M44 GLfloat)
    , model    :: !(M44 GLfloat)
    } deriving Show

main :: IO ()
main = do
    let conf = Configuration { versionMajor = 3
                             , versionMinor = 3
                             , displayMode = SizedScreen (1024, 768)
                             , windowCaption = "Rotating Triangle"
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
                    [ (VertexShader, "rotating-triangle/vertex.glsl")
                    , (FragmentShader, "rotating-triangle/fragment.glsl")
                    ]
    case eProgram of
        Right program' -> do
            mvpLoc' <- Program.getUniformLocation program' "mvp"
            mesh' <- Mesh.fromVector StaticDraw vertices indices
            (width, height) <- displayDimensions

            setWindowSizeCallback (Just windowSizeCallback)

            GL.glClearColor 0 0 0.4 0

            return $ Right State
                { program = program'
                , mvpLoc = mvpLoc'
                , mesh = mesh'
                , rotation = 0
                , persp = makePerspective width height
                , view = lookAt (V3 0 0 5) (V3 0 0 0) (V3 0 1 0)
                , model = makeRotation 0
                }

        Left err -> return $ Left err

animateCallback :: Render State ()
animateCallback = do
    frameTime <- frameDuration
    modifyAppState (\state ->
        let newRotation = (rotation state) + realToFrac frameTime * pi
            newModel = makeRotation newRotation
        in state { rotation = newRotation, model = newModel }
        )

renderCallback :: Render State ()
renderCallback = do
    state <- getAppStateUnsafe

    GL.glClear GL.GL_COLOR_BUFFER_BIT

    Program.enable (program state)
    Mesh.enable (mesh state)

    let mvp = persp state !*! view state !*! model state
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

vertices :: Vector Vertex
vertices =
    fromList
        [ Vertex { position = V3   0    1  0, color = V4 1 0 0 1 }
        , Vertex { position = V3 (-1) (-1) 0, color = V4 0 1 0 1 }
        , Vertex { position = V3   1  (-1) 0, color = V4 0 0 1 1 }
        ]

indices :: Vector GLuint
indices = fromList [0, 1, 2]

makePerspective :: Int -> Int -> M44 GLfloat
makePerspective width height =
    perspective (toRadians 45) (fromIntegral width / fromIntegral height) 0.001 1000

makeRotation :: GLfloat -> M44 GLfloat
makeRotation theta = mkTransformation (axisAngle (V3 0 1 0) theta) zero
