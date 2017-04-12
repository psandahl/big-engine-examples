module Main where

import           BigE.Attribute.Vert_P (Vertex (..))
import           BigE.Math             (toRadians)
import           BigE.Mesh             (Mesh)
import qualified BigE.Mesh             as Mesh
import qualified BigE.Program          as Program
import           BigE.Runtime
import           BigE.Texture          (TextureParameters (..), defaultParams2D)
import qualified BigE.Texture          as Texture
import           BigE.Types
import           Data.Vector.Storable  (Vector, fromList)
import           Graphics.GL           (GLfloat, GLuint)
import qualified Graphics.GL           as GL
import           Linear                (M44, V3 (..), identity, lookAt,
                                        perspective, (!*!))

data State = State
    { program  :: !Program
    , vpLoc    :: !Location
    , modelLoc :: !Location
    , mesh     :: !Mesh
    , persp    :: !(M44 GLfloat)
    , view     :: !(M44 GLfloat)
    , model    :: !(M44 GLfloat)
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
            vpLoc' <- Program.getUniformLocation program' "vp"
            modelLoc' <- Program.getUniformLocation program' "model"
            mesh' <- Mesh.fromVector StaticDraw vertices indices
            (width, height) <- displayDimensions

            setWindowSizeCallback $ Just windowSizeCallback

            GL.glClearColor 1 1 1 0

            return $ Right State
                { program = program'
                , vpLoc = vpLoc'
                , modelLoc = modelLoc'
                , mesh = mesh'
                , persp = makePerspective width height
                , view = lookAt (V3 0 0 5) (V3 0 0 (-1)) (V3 0 1 0)
                , model = identity
                }

        Left err -> return $ Left err

animateCallback :: Render State ()
animateCallback = return ()

renderCallback :: Render State ()
renderCallback = do
    GL.glClear GL.GL_COLOR_BUFFER_BIT

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

vertices :: Vector Vertex
vertices =
    fromList
        [ Vertex { position = V3   1    1  (-1) }
        , Vertex { position = V3 (-1)   1  (-1) }
        , Vertex { position = V3 (-1) (-1) (-1) }
        , Vertex { position = V3   1  (-1) (-1) }
        ]

indices :: Vector GLuint
indices =
    fromList
        [ -- Negative z
          0, 1, 2, 0, 2, 3
        ]

makePerspective :: Int -> Int -> M44 GLfloat
makePerspective width height =
    perspective (toRadians 45) (fromIntegral width / fromIntegral height) 0.001 1000
