module Main where

import           BigE.Attribute.Vert_P  (Vertex (..))
import           BigE.Mesh              (Mesh)
import qualified BigE.Mesh              as Mesh
import qualified BigE.Program           as Program
import           BigE.Runtime
import           BigE.Types
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Vector.Storable   (Vector, fromList)
import           Graphics.GL            (GLuint)
import qualified Graphics.GL            as GL
import           Linear                 (V3 (..))

data State = State
    { program :: !Program
    , mesh    :: !Mesh
    } deriving Show

main :: IO ()
main = do
    let conf = Configuration { versionMajor = 3
                             , versionMinor = 3
                             , displayMode = SizedScreen (1024, 768)
                             , windowCaption = "Untransformed Triangle"
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
                 [ (VertexShader, "untransformed-triangle/vertex.glsl")
                 , (FragmentShader, "untransformed-triangle/fragment.glsl")
                 ]

    case eProg of
        Right prog -> do
            GL.glClearColor 0 0 0.4 0
            mesh' <- Mesh.fromVector StaticDraw vertices indices
            return $ Right State { program = prog, mesh = mesh' }

        Left err -> return $ Left err


renderCallback :: Render State ()
renderCallback = do
    state <- getAppStateUnsafe

    GL.glClear GL.GL_COLOR_BUFFER_BIT
    Program.enable (program state)
    Mesh.enable (mesh state)

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
    fromList
        [ Vertex { position = V3   0    1  0 }
        , Vertex { position = V3 (-1) (-1) 0 }
        , Vertex { position = V3   1  (-1) 0 }
        ]

indices :: Vector GLuint
indices = fromList [0, 1, 2]
