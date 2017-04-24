module Main where


import           BigE.Attribute.Vert_P (Vertex (..))
import           BigE.Mesh             (Mesh)
import qualified BigE.Mesh             as Mesh
import qualified BigE.Program          as Program
import           BigE.Runtime
import           BigE.Types
import           Data.Vector.Storable  (Vector, fromList)
import           Graphics.GL           (GLfloat, GLuint)
import qualified Graphics.GL           as GL
import           Linear                (M44, V3 (..), ortho)

data State = State
    { program  :: !Program
    , mesh     :: !Mesh
    , perspLoc :: !Location
    , persp    :: !(M44 GLfloat)
    } deriving Show

main :: IO ()
main = do
    let conf = Configuration { versionMajor = 3
                             , versionMinor = 3
                             , displayMode = SizedScreen (1024, 768)
                             , windowCaption = "Text Rendering"
                             , setup = setupCallback
                             , animate = return ()
                             , render = renderCallback
                             , teardown = teardownCallback
                             }
    result <- runBigE conf
    print result

setupCallback :: Render State (Either String State)
setupCallback = do
    eProg <- Program.fromFile
                 [ (VertexShader, "text-rendering/vertex.glsl")
                 , (FragmentShader, "text-rendering/fragment.glsl")
                 ]

    case eProg of
        Right prog -> do
            GL.glClearColor 0 0 0.4 0
            mesh' <- Mesh.fromVector StaticDraw vertices indices
            perspLoc' <- Program.getUniformLocation prog "persp"

            return $ Right State
                { program = prog
                , mesh = mesh'
                , perspLoc = perspLoc'
                , persp = ortho (-10) 10 (-10) 10 (-0.1) 0.1
                }

        Left err -> return $ Left err

renderCallback :: Render State ()
renderCallback = do
    state <- getAppStateUnsafe

    GL.glClear GL.GL_COLOR_BUFFER_BIT
    Program.enable (program state)
    Mesh.enable (mesh state)

    setUniform (perspLoc state) (persp state)

    Mesh.render Triangles (mesh state)

    Mesh.disable
    Program.disable

teardownCallback :: Render State ()
teardownCallback = do
    state <- getAppStateUnsafe
    Mesh.delete (mesh state)
    Program.delete (program state)

vertices :: Vector Vertex
vertices =
    fromList $ quadAt (0, 0) 0.25 0.25

indices :: Vector GLuint
indices = fromList [0, 1, 2, 0, 2, 3]

quadAt :: (GLfloat, GLfloat) -> GLfloat -> GLfloat -> [Vertex]
quadAt (leftUpX, leftUpY) width height =
    [ Vertex { position = V3 (leftUpX + width) leftUpY 0 }
    , Vertex { position = V3 leftUpX leftUpY 0 }
    , Vertex { position = V3 leftUpX (leftUpY - height) 0}
    , Vertex { position = V3 (leftUpX + width) (leftUpY - height) 0}
    ]
