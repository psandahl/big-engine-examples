module Main where

import           BigE.Attribute.Vert_P_Tx (Vertex (..))
import           BigE.Math                (toRadians)
import           BigE.Mesh                (Mesh)
import qualified BigE.Mesh                as Mesh
import qualified BigE.Program             as Program
import           BigE.Runtime
import           BigE.Texture             (TextureParameters (..),
                                           defaultParams2D)
import qualified BigE.Texture             as Texture
import           BigE.Types
import           Control.Monad.IO.Class   (MonadIO, liftIO)
import           Data.Vector.Storable     (Vector, fromList)
import           Graphics.GL              (GLfloat, GLint, GLuint)
import qualified Graphics.GL              as GL
import           Linear                   (M44, V2 (..), V3 (..), lookAt,
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
    result <- runBigE conf
    print result

setupCallback :: Render State (Either String State)
setupCallback = do
    eProgram <- Program.fromFile
                    [ (VertexShader, "transparant-cat/vertex.glsl")
                    , (FragmentShader, "transparant-cat/fragment.glsl")
                    ]
    case eProgram of
        Right program' -> do
            eTexture <-
                Texture.fromFile2D "transparant-cat/cat.png"
                                   defaultParams2D { format = RGBA8 }
            case eTexture of
                Right texture -> do
                    mvpLoc' <- Program.getUniformLocation program' "mvp"
                    catTextureLoc' <- Program.getUniformLocation program' "catTexture"
                    mesh' <- Mesh.fromVector StaticDraw vertices indices
                    (width, height) <- displayDimensions

                    setWindowSizeCallback $ Just windowSizeCallback

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

    Program.enable (program state)
    Mesh.enable (mesh state)

    let mvp = persp state !*! view state
    setUniform (mvpLoc state) mvp

    Texture.enable2D 0 (catTexture state)
    setUniform (catTextureLoc state) (0 :: GLint)

    Mesh.render Triangles (mesh state)

    Texture.disable2D 0
    Mesh.disable
    Program.disable

teardownCallback :: Render State ()
teardownCallback = do
    state <- getAppStateUnsafe
    Mesh.delete (mesh state)
    Texture.delete (catTexture state)
    Program.delete (program state)

windowSizeCallback :: Int -> Int -> Render State ()
windowSizeCallback width height =
    modifyAppState (\state -> state { persp = makePerspective width height })

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
    perspective (toRadians 45) (fromIntegral width / fromIntegral height) 0.001 1000
