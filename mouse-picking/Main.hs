module Main where

import           BigE.Attribute.Vert_P_Tx (Vertex (..))
import           BigE.Math                (toRadians)
import           BigE.Mesh                (Mesh)
import qualified BigE.Mesh                as Mesh
import           BigE.MousePicker         (MousePicker (..), ObjectId,
                                           PickObject (..), Pickable (..),
                                           mkObjectId)
import qualified BigE.MousePicker         as MousePicker
import qualified BigE.Program             as Program
import           BigE.Runtime             (Configuration (..), DisplayMode (..),
                                           Render, displayDimensions,
                                           frameDuration, getAppStateUnsafe,
                                           modifyAppState, runBigE,
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
    , objId      :: !ObjectId
    } deriving Show

instance PickObject Entity where
    objectId = objId
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
    , boxModel   :: !(M44 GLfloat)
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

                    mesh <- Mesh.fromVector StaticDraw vertices indices
                    entMvpLoc' <- Program.getUniformLocation entProgram' "mvp"
                    boxMvpLoc' <- Program.getUniformLocation boxProgram' "mvp"
                    boxTexLoc' <- Program.getUniformLocation boxProgram' "tex"

                    let ent1 = Entity { entProgram = entProgram'
                                      , entMvpLoc = entMvpLoc'
                                      , entMesh = mesh
                                      , model = makeTranslation (V3 (-3) (-3) 0)
                                      , objId = mkObjectId 255
                                      }
                        ent2 = Entity { entProgram = entProgram'
                                      , entMvpLoc = entMvpLoc'
                                      , entMesh = mesh
                                      , model = makeTranslation (V3 1 (-1) 0)
                                      , objId = mkObjectId 65280
                                      }

                    return $ Right State
                        { boxProgram = boxProgram'
                        , boxMvpLoc = boxMvpLoc'
                        , boxTexLoc = boxTexLoc'
                        , boxMesh = mesh
                        , boxModel = makeTranslation (V3 0 2 0)
                        , picker = picker'
                        , persp = makePerspective width height
                        , view = lookAt (V3 0 0 10) (V3 0 0 0) (V3 0 1 0)
                        , entities = [ent1, ent2]
                        , frameTime = 0
                        , frames = 0
                        }

                Left err -> return $ Left err

        Left err -> return $ Left err

renderCallback :: Render State ()
renderCallback = do
    state <- getAppStateUnsafe

    let vp = persp state !*! view state

    -- First, render the entities to the mouse picker.
    MousePicker.render vp (map Pickable $ entities state) (picker state)

    -- Now proceed with ordinary rendering.
    GL.glEnable GL.GL_DEPTH_FUNC
    GL.glClearColor 0 0 0.4 0
    GL.glClear (GL.GL_COLOR_BUFFER_BIT .|. GL.GL_DEPTH_BUFFER_BIT)

    -- Render box
    Program.enable (boxProgram state)
    Mesh.enable (boxMesh state)

    let mvp = vp !*! boxModel state
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