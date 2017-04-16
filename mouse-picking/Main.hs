module Main where

import           BigE.Attribute.Vert_P_Tx (Vertex (..))
import           BigE.Math                (toRadians)
import           BigE.Mesh                (Mesh)
import qualified BigE.Mesh                as Mesh
import qualified BigE.Program             as Program
import           BigE.Runtime             (Configuration (..), DisplayMode (..),
                                           Render, displayDimensions,
                                           getAppStateUnsafe, modifyAppState,
                                           runBigE, setWindowSizeCallback)
import           BigE.Types
import           Control.Monad            (forM_)
import           Control.Monad.IO.Class   (liftIO)
import           Data.Vector.Storable     (Vector, fromList)
import           Graphics.GL              (GLfloat, GLuint)
import qualified Graphics.GL              as GL
import           Linear                   (M44, V2 (..), V3 (..), V4 (..),
                                           identity, lookAt, perspective, (!*!))

data Entity = Entity
    { entProgram :: !Program
    , entMvpLoc  :: !Location
    , entMesh    :: !Mesh
    , model      :: !(M44 GLfloat)
    } deriving Show

data State = State
    { persp    :: !(M44 GLfloat)
    , view     :: !(M44 GLfloat)
    , entities :: ![Entity]
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
    eEntProgram <- Program.fromFile
                      [ (VertexShader, "mouse-picking/entvertex.glsl")
                      , (FragmentShader, "mouse-picking/entfragment.glsl")
                      ]

    case eEntProgram of
        Right entProgram' -> do

            mesh <- Mesh.fromVector StaticDraw vertices indices
            entMvpLoc' <- Program.getUniformLocation entProgram' "mvp"
            (width, height) <- displayDimensions

            let ent1 = Entity { entProgram = entProgram'
                              , entMvpLoc = entMvpLoc'
                              , entMesh = mesh
                              , model = makeTranslation (V3 (-3) (-3) 0)
                              }
                ent2 = Entity { entProgram = entProgram'
                              , entMvpLoc = entMvpLoc'
                              , entMesh = mesh
                              , model = makeTranslation (V3 1 (-1) 0)
                              }

            GL.glClearColor 0 0 0.4 0

            return $ Right State
                { persp = makePerspective width height
                , view = lookAt (V3 0 0 10) (V3 0 0 0) (V3 0 1 0)
                , entities = [ent1, ent2]
                }

        Left err -> return $ Left err

renderCallback :: Render State ()
renderCallback = do
    state <- getAppStateUnsafe

    let vp = persp state !*! view state

    GL.glClear GL.GL_COLOR_BUFFER_BIT

    forM_ (entities state) $ \entity -> do
        Program.enable (entProgram entity)
        Mesh.enable (entMesh entity)

        let mvp = vp !*! model entity
        setUniform (entMvpLoc entity) mvp
        Mesh.render Triangles (entMesh entity)

        Mesh.disable
        Program.disable

teardownCallback :: Render State ()
teardownCallback = do
    liftIO $ putStrLn "teardown"
    state <- getAppStateUnsafe
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
