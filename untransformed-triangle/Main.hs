module Main where

import           BigE.Attribute.Vert_P  (Vertex (..))
import           BigE.MeshLoader        (Mesh)
import qualified BigE.MeshLoader        as MeshLoader
import qualified BigE.ProgramLoader     as ProgramLoader
import           BigE.Runtime
import           BigE.Types
import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.ByteString.Char8  as BS
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
    eProg <- loadProgram

    case eProg of
        Right prog -> do
            GL.glClearColor 0 0 0.4 0
            mesh' <- MeshLoader.fromVector StaticDraw vertices indices
            return $ Right State { program = prog, mesh = mesh' }

        Left err -> return $ Left err


renderCallback :: Render State ()
renderCallback = do
    state <- getAppStateUnsafe

    GL.glClear GL.GL_COLOR_BUFFER_BIT
    ProgramLoader.enable (program state)
    MeshLoader.enable (mesh state)

    MeshLoader.render Triangles (mesh state)

    MeshLoader.disable
    ProgramLoader.disable

teardownCallback :: Render State ()
teardownCallback = do
    state <- getAppStateUnsafe

    MeshLoader.delete (mesh state)
    ProgramLoader.delete (program state)

loadProgram :: MonadIO m => m (Either String Program)
loadProgram = do
    vs <- liftIO $ BS.readFile "untransformed-triangle/vertex.glsl"
    fs <- liftIO $ BS.readFile "untransformed-triangle/fragment.glsl"
    ProgramLoader.fromByteString
        [ (VertexShader, "untransformed-triangle/vertex.glsl", vs)
        , (FragmentShader, "untransformed-triangle/fragment.glsl", fs)
        ]

vertices :: Vector Vertex
vertices =
    fromList
        [ Vertex { position = V3   0    1  0 }
        , Vertex { position = V3 (-1) (-1) 0 }
        , Vertex { position = V3   1  (-1) 0 }
        ]

indices :: Vector GLuint
indices = fromList [0, 1, 2]
