module Main where

import           Control.Monad.IO.Class   (MonadIO, liftIO)
import qualified Data.ByteString.Char8    as BS
import           Data.Vector.Storable     (Vector, fromList)
import           Graphics.Big
import           Graphics.Big.Mesh.Vert_P (Vertex (..))
import           Graphics.GL              (GLuint)
import qualified Graphics.GL              as GL
import           Linear                   (V3 (..))

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
    result <- runEngine conf
    print result

setupCallback :: Render State (Either String State)
setupCallback = do
    eProg <- loadProgram

    case eProg of
        Right prog -> do
            GL.glClearColor 0 0 0.4 0
            mesh' <- fromVectors StaticDraw vertices indices
            return $ Right State { program = prog, mesh = mesh' }

        Left err -> return $ Left err


renderCallback :: Render State ()
renderCallback = do
    state <- getAppStateUnsafe

    GL.glClear GL.GL_COLOR_BUFFER_BIT
    enableProgram (program state)
    enableMesh (mesh state)

    renderMesh Triangles (mesh state)

    disableMesh
    disableProgram


teardownCallback :: Render State ()
teardownCallback = do
    state <- getAppStateUnsafe

    deleteMesh (mesh state)
    deleteProgram (program state)

loadProgram :: MonadIO m => m (Either String Program)
loadProgram = do
    vs <- liftIO $ BS.readFile "untransformed-triangle/vertex.glsl"
    fs <- liftIO $ BS.readFile "untransformed-triangle/fragment.glsl"
    fromByteString
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
