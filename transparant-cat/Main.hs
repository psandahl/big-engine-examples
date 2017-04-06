module Main where

import           Control.Monad.IO.Class      (MonadIO, liftIO)
import qualified Data.ByteString.Char8       as BS
import           Data.Vector.Storable        (Vector, fromList)
import           Graphics.Big
import           Graphics.Big.Mesh.Vert_P_Tx (Vertex (..))
import           Graphics.GL                 (GLfloat, GLint, GLuint)
import qualified Graphics.GL                 as GL
import           Linear                      (M44, V2 (..), V3 (..), lookAt,
                                              perspective, (!*!))

data State = State
    { program       :: !Program
    , mvpLoc        :: !Location
    , catTextureLoc :: !Location
    , mesh          :: !Mesh
    , catTexture    :: !Texture
    , persp         :: !(M44 GLfloat)
    , view          :: !(M44 GLfloat)
    } deriving Show

main :: IO ()
main = do
    let conf = Configuration { versionMajor = 3
                             , versionMinor = 3
                             , displayMode = SizedScreen (1024, 768)
                             , windowCaption = "Transparant Cat"
                             , setup = setupCallback
                             , animate = animateCallback
                             , render = renderCallback
                             , teardown = teardownCallback
                             }
    result <- runEngine conf
    print result

setupCallback :: Render State (Either String State)
setupCallback = do
    eProgram <- loadProgram
    case eProgram of
        Right program' -> do
            eTexture <-
                texture2DFromFile "transparant-cat/cat.png"
                                  defaultTextureParameters { format = RGBA8 }
            case eTexture of
                Right texture -> do
                    mvpLoc' <- getUniformLocation program' "mvp"
                    catTextureLoc' <- getUniformLocation program' "catTexture"
                    mesh' <- meshFromVectors StaticDraw vertices indices
                    (width, height) <- displayDimensions

                    GL.glClearColor 1 1 1 0

                    return $ Right State
                        { program = program'
                        , mvpLoc = mvpLoc'
                        , catTextureLoc = catTextureLoc'
                        , mesh = mesh'
                        , catTexture = texture
                        , persp = makePerspective width height
                        , view = lookAt (V3 0 0 5) (V3 0 0 0) (V3 0 1 0)
                        }

                Left err -> return $ Left err

        Left err -> return $ Left err

animateCallback :: Render State ()
animateCallback = return ()

renderCallback :: Render State ()
renderCallback = do
    GL.glClear GL.GL_COLOR_BUFFER_BIT

    state <- getAppStateUnsafe

    enableProgram (program state)
    enableMesh (mesh state)

    let mvp = persp state !*! view state
    setUniform (mvpLoc state) mvp

    bindTexture2D 0 (catTexture state)
    setUniform (catTextureLoc state) (0 :: GLint)

    renderMesh Triangles (mesh state)

    disableTexture2D
    disableMesh
    disableProgram

teardownCallback :: Render State ()
teardownCallback = do
    state <- getAppStateUnsafe
    deleteMesh (mesh state)
    deleteTexture (catTexture state)
    deleteProgram (program state)

loadProgram :: MonadIO m => m (Either String Program)
loadProgram = do
        vs <- liftIO $ BS.readFile "transparant-cat/vertex.glsl"
        fs <- liftIO $ BS.readFile "transparant-cat/fragment.glsl"
        programFromByteStrings
            [ (VertexShader, "transparant-cat/vertex.glsl", vs)
            , (FragmentShader, "transparant-cat/fragment.glsl", fs)
            ]

vertices :: Vector Vertex
vertices =
    fromList
        [ Vertex { position = V3   1    1  0, texCoord = V2 1 1 }
        , Vertex { position = V3 (-1)   1  0, texCoord = V2 0 1 }
        , Vertex { position = V3 (-1) (-1) 0, texCoord = V2 0 0 }
        , Vertex { position = V3   1  (-1) 0, texCoord = V2 1 0 }
        ]

indices :: Vector GLuint
indices =
    fromList
        [ 0, 1, 2, 0, 2, 3 ]

makePerspective :: Int -> Int -> M44 GLfloat
makePerspective width height =
    perspective (degToRad 45) (fromIntegral width / fromIntegral height) 0.001 1000

degToRad :: Floating a => a -> a
degToRad d = d * (pi / 180)
