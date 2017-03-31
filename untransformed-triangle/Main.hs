module Main where

import           Control.Monad.IO.Class   (liftIO)
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
                             , eachFrame = frameCallback
                             , teardown = teardownCallback
                             }
    result <- runEngine conf
    print result

setupCallback :: Render State (Either String State)
setupCallback = do
    vs <- liftIO $ BS.readFile "untransformed-triangle/vertex.glsl"
    fs <- liftIO $ BS.readFile "untransformed-triangle/fragment.glsl"
    eProg <-
        fromByteString
            [ (VertexShader, "untransformed-triangle/vertex.glsl", vs)
            , (FragmentShader, "untransformed-triangle/fragment.glsl", fs)
            ]

    case eProg of
        Right prog -> do
            GL.glClearColor 0 0 0.4 0
            mesh' <- fromVectors StaticDraw vertices indices
            return $ Right State { program = prog, mesh = mesh' }

        Left err -> return $ Left err


frameCallback :: Render State ()
frameCallback = do
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

vertices :: Vector Vertex
vertices =
    fromList
        [ Vertex { position = V3   0    1  0 }
        , Vertex { position = V3 (-1) (-1) 0 }
        , Vertex { position = V3   1  (-1) 0 }
        ]

indices :: Vector GLuint
indices = fromList [0, 1, 2]
