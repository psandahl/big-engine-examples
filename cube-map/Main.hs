module Main where

import           BigE.Attribute.Vert_P (Vertex (..))
import           BigE.Math             (toRadians)
import           BigE.Mesh             (Mesh)
import qualified BigE.Mesh             as Mesh
import qualified BigE.Program          as Program
import           BigE.Runtime
import           BigE.Texture          (CubeMapFiles (..), defaultParamsCube)
import qualified BigE.Texture          as Texture
import           BigE.Types
import           Data.Bits             ((.|.))
import           Data.Vector.Storable  (Vector, fromList)
import           Graphics.GL           (GLfloat, GLuint)
import qualified Graphics.GL           as GL
import           Linear                (M44, V3 (..), axisAngle, lookAt,
                                        mkTransformation, perspective, zero,
                                        (!*!))

data State = State
    { program        :: !Program
    , vpLoc          :: !Location
    , modelLoc       :: !Location
    , cubeTextureLoc :: !Location
    , mesh           :: !Mesh
    , cubeTexture    :: !Texture
    , persp          :: !(M44 GLfloat)
    , view           :: !(M44 GLfloat)
    , cubeRotation   :: !GLfloat
    , model          :: !(M44 GLfloat)
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

            let files = CubeMapFiles { negativeX = "cube-map/right.png"
                                     , positiveX = "cube-map/left.png"
                                     , negativeY = "cube-map/bottom.png"
                                     , positiveY = "cube-map/top.png"
                                     , negativeZ = "cube-map/back.png"
                                     , positiveZ = "cube-map/front.png"
                                     }
            eTexture <- Texture.fromFileCube files defaultParamsCube
            case eTexture of

                Right texture -> do

                    vpLoc' <- Program.getUniformLocation program' "vp"
                    modelLoc' <- Program.getUniformLocation program' "model"
                    cubeTextureLoc' <- Program.getUniformLocation program' "cube"
                    mesh' <- Mesh.fromVector StaticDraw (vertices 1) indices
                    (width, height) <- displayDimensions

                    setWindowSizeCallback $ Just windowSizeCallback

                    GL.glClearColor 1 1 1 0
                    GL.glEnable GL.GL_DEPTH

                    return $ Right State
                        { program = program'
                        , vpLoc = vpLoc'
                        , modelLoc = modelLoc'
                        , cubeTextureLoc = cubeTextureLoc'
                        , mesh = mesh'
                        , cubeTexture = texture
                        , persp = makePerspective width height
                        , view = lookAt (V3 0 0 3) (V3 0 0 (-1)) (V3 0 1 0)
                        , cubeRotation = 0
                        , model = makeRotation (-(pi / 8))
                        }

                Left err -> return $ Left err

        Left err -> return $ Left err

animateCallback :: Render State ()
animateCallback = return ()
    -- duration <- frameDuration
    -- modifyAppState $ \state ->
    --     let newRotation = (realToFrac duration) * 0.05 * pi + (cubeRotation state)
    --     in state { cubeRotation = newRotation
    --              , model = makeRotation newRotation
    --              }

renderCallback :: Render State ()
renderCallback = do
    GL.glClear (GL.GL_COLOR_BUFFER_BIT .|. GL.GL_DEPTH_BUFFER_BIT)

    state <- getAppStateUnsafe

    Program.enable (program state)
    Mesh.enable (mesh state)

    let vp = persp state !*! view state
    setUniform (vpLoc state) vp
    setUniform (modelLoc state) (model state)

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
          -- Negative x
        , 1, 5, 6, 1, 6, 2
          -- Positive x
        , 4, 0, 3, 4, 3, 7
          -- Positive y
        , 0, 1, 4, 1, 4, 5
        ]

makePerspective :: Int -> Int -> M44 GLfloat
makePerspective width height =
    perspective (toRadians 45) (fromIntegral width / fromIntegral height) 0.001 1000


makeRotation :: GLfloat -> M44 GLfloat
makeRotation theta = mkTransformation (axisAngle (V3 0 1 0) theta) zero