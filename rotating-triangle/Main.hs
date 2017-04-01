module Main where

import           Control.Monad.IO.Class   (MonadIO, liftIO)
import           Data.ByteString.Char8    as BS
import           Data.Vector.Storable     (Vector, fromList)
import           Graphics.Big
import           Graphics.Big.Mesh.Vert_P (Vertex (..))
import           Graphics.GL              (GLfloat, GLuint)
import qualified Graphics.GL              as GL
import           Linear                   (M44, V3 (..), axisAngle, lookAt,
                                           mkTransformation, perspective, zero,
                                           (!*!))

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
                             , eachFrame = frameCallback
                             , teardown = teardownCallback
                             }
    result <- runEngine conf
    print result

setupCallback :: Render State (Either String State)
setupCallback = do
    eProgram <- loadProgram
    case eProgram of
        Right program' -> do
            mvpLoc' <- getUniformLocation program' "mvp"
            mesh' <- fromVectors StaticDraw vertices indices
            (width, height) <- displayDimension

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

frameCallback :: Render State ()
frameCallback = do
    frameTime <- frameDuration
    modifyAppState (\state ->
        let newRotation = (rotation state) + realToFrac frameTime * pi
            newModel = makeRotation newRotation
        in state { rotation = newRotation, model = newModel }
        )

    state <- getAppStateUnsafe

    GL.glClear GL.GL_COLOR_BUFFER_BIT

    enableProgram (program state)
    enableMesh (mesh state)

    let mvp = persp state !*! view state !*! model state
    setUniform (mvpLoc state) mvp
    renderMesh Triangles (mesh state)

    disableMesh
    disableProgram

teardownCallback :: Render State ()
teardownCallback = do
    state <- getAppStateUnsafe
    deleteMesh (mesh state)
    deleteProgram (program state)

windowSizeCallback :: Int -> Int -> Render State ()
windowSizeCallback width height =
    modifyAppState (\state -> state { persp = makePerspective width height })

loadProgram :: MonadIO m => m (Either String Program)
loadProgram = do
    vs <- liftIO $ BS.readFile "rotating-triangle/vertex.glsl"
    fs <- liftIO $ BS.readFile "rotating-triangle/fragment.glsl"
    fromByteString
        [ (VertexShader, "rotating-triangle/vertex.glsl", vs)
        , (FragmentShader, "rotating-triangle/fragment.glsl", fs)
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

makePerspective :: Int -> Int -> M44 GLfloat
makePerspective width height =
    perspective (degToRad 45) (fromIntegral width / fromIntegral height) 0.001 1000

makeRotation :: GLfloat -> M44 GLfloat
makeRotation theta = mkTransformation (axisAngle (V3 0 1 0) theta) zero

degToRad :: Floating a => a -> a
degToRad d = d * (pi / 180)
