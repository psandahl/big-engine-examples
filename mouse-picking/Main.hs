module Main where

import           BigE.Attribute.Vert_P_Tx (Vertex (..))
import           BigE.Math                (toRadians)
import           BigE.Mesh                (Mesh)
import qualified BigE.Mesh                as Mesh
import           BigE.MousePicker         (MousePicker (..), PickId,
                                           PickObject (..), Pickable (..),
                                           literalPickId)
import qualified BigE.MousePicker         as MousePicker
import qualified BigE.Program             as Program
import           BigE.Runtime             (Configuration (..), DisplayMode (..),
                                           ModifierKeys, MouseButton, Render,
                                           displayDimensions, frameDuration,
                                           getAppStateUnsafe, modifyAppState,
                                           putAppState, runBigE,
                                           setMousePressedCallback,
                                           setWindowSizeCallback)
import qualified BigE.Texture             as Texture
import           BigE.Types
import           Control.Monad            (forM_, unless)
import           Control.Monad.IO.Class   (liftIO)
import           Data.Bits                ((.|.))
import           Data.Vector.Storable     (Vector, fromList)
import           Graphics.GL              (GLfloat, GLint, GLuint)
import qualified Graphics.GL              as GL
import           Linear                   (M44, V2 (..), V3 (..), V4 (..),
                                           lookAt, perspective, (!*!))

data Entity = Entity
    { entProgram :: !Program
    , entMvpLoc  :: !Location
    , entMesh    :: !Mesh
    , model      :: !(M44 GLfloat)
    , pid        :: !PickId
    } deriving Show

instance PickObject Entity where
    pickId = pid
    modelMatrix = model
    renderForPicking ent = do
        Mesh.enable (entMesh ent)
        Mesh.render Triangles (entMesh ent)
        Mesh.disable

data State = State
    { boxProgram :: !Program
    , boxMvpLoc  :: !Location
    , boxTexLoc  :: !Location
    , boxMesh    :: !Mesh
    , boxTrans   :: !(M44 GLfloat)
    , picker     :: !MousePicker
    , persp      :: !(M44 GLfloat)
    , view       :: !(M44 GLfloat)
    , entities   :: ![Entity]
    , frameTime  :: !Double
    , frames     :: !Int
    } deriving Show

main :: IO ()
main = do
    let conf = Configuration { versionMajor = 3
                             , versionMinor = 3
                             , displayMode = SizedScreen (1024, 768)
                             , windowCaption = "Mouse Picking"
                             , setup = setupCallback
                             , animate = return ()
                             , render = renderCallback
                             , teardown = teardownCallback
                             }
    res <- runBigE conf
    print res

setupCallback :: Render State (Either String State)
setupCallback = do
    eProgram <-
        sequence <$> mapM Program.fromFile
            [
              [ (VertexShader, "mouse-picking/entvertex.glsl")
              , (FragmentShader, "mouse-picking/entfragment.glsl")
              ]
            , [ (VertexShader, "mouse-picking/boxvertex.glsl")
              , (FragmentShader, "mouse-picking/boxfragment.glsl")
              ]
            ]

    case eProgram of
        Right [entProgram', boxProgram'] -> do
            (width, height) <- displayDimensions

            ePicker <- MousePicker.init width height
            case ePicker of
                Right picker' -> do

                    setWindowSizeCallback (Just windowSizeCallback)
                    setMousePressedCallback (Just mousePressedCallback)

                    mesh <- Mesh.fromVector StaticDraw vertices indices
                    entMvpLoc' <- Program.getUniformLocation entProgram' "mvp"
                    boxMvpLoc' <- Program.getUniformLocation boxProgram' "mvp"
                    boxTexLoc' <- Program.getUniformLocation boxProgram' "tex"

                    let ent1 = Entity { entProgram = entProgram'
                                      , entMvpLoc = entMvpLoc'
                                      , entMesh = mesh
                                      , model = makeTranslation (V3 (-3) (-3) 0)
                                      , pid = literalPickId 0 0 255
                                      }
                        ent2 = Entity { entProgram = entProgram'
                                      , entMvpLoc = entMvpLoc'
                                      , entMesh = mesh
                                      , model = makeTranslation (V3 1 (-1) 0)
                                      , pid = literalPickId 0 255 0
                                      }
                        ent3 = Entity { entProgram = entProgram'
                                      , entMvpLoc = entMvpLoc'
                                      , entMesh = mesh
                                      , model = makeTranslation (V3 0 0 (-3))
                                      , pid = literalPickId 255 0 0
                                      }

                    return $ Right State
                        { boxProgram = boxProgram'
                        , boxMvpLoc = boxMvpLoc'
                        , boxTexLoc = boxTexLoc'
                        , boxMesh = mesh
                        , boxTrans = makeTranslation (V3 0 2 0)
                        , picker = picker'
                        , persp = makePerspective width height
                        , view = lookAt (V3 0 0 10) (V3 0 0 0) (V3 0 1 0)
                        , entities = [ent1, ent2, ent3]
                        , frameTime = 0
                        , frames = 0
                        }

                Left err -> return $ Left err

        Right _ -> return $ Left "Unexpected number of shader programs"

        Left err -> return $ Left err

renderCallback :: Render State ()
renderCallback = do
    state <- getAppStateUnsafe

    let vp = persp state !*! view state

    -- First, render the entities to the mouse picker.
    MousePicker.render vp (map Pickable $ entities state) (picker state)

    -- Now proceed with ordinary rendering.
    GL.glEnable GL.GL_DEPTH_TEST
    GL.glClearColor 0 0 0.4 0
    GL.glClear (GL.GL_COLOR_BUFFER_BIT .|. GL.GL_DEPTH_BUFFER_BIT)

    -- Render box.
    (width, height) <- displayDimensions
    GL.glViewport 0 0 (fromIntegral width) (fromIntegral height)
    Program.enable (boxProgram state)
    Mesh.enable (boxMesh state)

    -- Make the shape of the box proportional to the screen by
    -- scaling its x axis.
    let xScale = fromIntegral width / fromIntegral height
        boxScale = makeScaling (V3 xScale 1 1)
        mvp = vp !*! boxTrans state !*! boxScale
    setUniform (boxMvpLoc state) mvp
    Texture.enable2D 0 (colorTexture $ picker state)
    setUniform (boxTexLoc state) (0 :: GLint)

    Mesh.render Triangles (boxMesh state)

    Texture.disable2D 0
    Mesh.disable
    Program.disable

    -- Render entities.
    forM_ (entities state) $ \entity -> do
        Program.enable (entProgram entity)
        Mesh.enable (entMesh entity)

        let mvp' = vp !*! model entity
        setUniform (entMvpLoc entity) mvp'
        Mesh.render Triangles (entMesh entity)

        Mesh.disable
        Program.disable

    duration <- frameDuration
    modifyAppState $ \s -> s { frameTime = duration + frameTime s
                             , frames = frames s + 1
                             }

teardownCallback :: Render State ()
teardownCallback = do
    state <- getAppStateUnsafe
    Program.delete $ boxProgram state
    let xs = entities state
    unless (null xs) $
        Program.delete (entProgram $ head xs)

    Mesh.delete $ boxMesh state
    MousePicker.delete $ picker state

    liftIO $ print state

windowSizeCallback :: Int -> Int -> Render State ()
windowSizeCallback width height = do
    state <- getAppStateUnsafe

    ePicker <- MousePicker.resize width height $ picker state
    case ePicker of
        Right picker' ->
            putAppState $ state { persp = makePerspective width height
                                , picker = picker'
                                }
        Left _err -> liftIO $ putStrLn "PANIC: MousePicker.resize"

mousePressedCallback :: MouseButton -> ModifierKeys -> (Int, Int) -> Render State ()
mousePressedCallback _button _modKeys (x, y) = do
    state <- getAppStateUnsafe
    (_, height) <- displayDimensions
    -- Rows need to be swapped between mouse coords and framebuffer coords.
    pid' <- MousePicker.getPickId (x, height - y) $ picker state
    liftIO $ print pid'

vertices :: Vector Vertex
vertices =
    fromList
        [ Vertex { position = V3 1 1 0, texCoord = V2 1 1 }
        , Vertex { position = V3 (-1) 1 0, texCoord = V2 0 1 }
        , Vertex { position = V3 (-1) (-1) 0, texCoord = V2 0 0 }
        , Vertex { position = V3 1 (-1) 0, texCoord = V2 1 0 }
        ]

indices :: Vector GLuint
indices =
    fromList
        [ 0, 1, 2, 0, 2, 3
        ]

makePerspective :: Int -> Int -> M44 GLfloat
makePerspective width height =
    perspective (toRadians 45) (fromIntegral width / fromIntegral height) 0.001 1000

makeTranslation :: V3 GLfloat -> M44 GLfloat
makeTranslation (V3 x y z) =
    V4 (V4 1 0 0 x)
       (V4 0 1 0 y)
       (V4 0 0 1 z)
       (V4 0 0 0 1)

makeScaling :: V3 GLfloat -> M44 GLfloat
makeScaling (V3 x y z) =
    V4 (V4 x 0 0 0)
       (V4 0 y 0 0)
       (V4 0 0 z 0)
       (V4 0 0 0 1)
