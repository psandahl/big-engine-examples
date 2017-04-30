module Main where

import           BigE.Attribute.Vert_P  (Vertex (..))
--import           BigE.Mesh              (Mesh)
import qualified BigE.Mesh              as Mesh
import qualified BigE.Program           as Program
import           BigE.Runtime
import           BigE.TextRenderer.Font (Font)
import qualified BigE.TextRenderer.Font as Font
import           BigE.TextRenderer.Text (Text (mesh))
import qualified BigE.TextRenderer.Text as Text
import           BigE.Types
import           Data.Either            (isLeft, isRight)
import           Data.Vector.Storable   (Vector, fromList)
import           Graphics.GL            (GLfloat, GLuint)
import qualified Graphics.GL            as GL
import           Linear                 (V3 (..))

data State = State
    { program :: !Program
    --, mesh    :: !Mesh
    , font    :: !Font
    , text    :: !Text
    } deriving Show

main :: IO ()
main = do
    let conf = Configuration { versionMajor = 3
                             , versionMinor = 3
                             , displayMode = SizedScreen (1024, 768)
                             , windowCaption = "Text Rendering"
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
                 [ (VertexShader, "text-rendering/vertex.glsl")
                 , (FragmentShader, "text-rendering/fragment.glsl")
                 ]
    eFont <- Font.fromFile "text-rendering/noto-bold.fnt"

    case eitherTwo (eProg, eFont) of
        Right (prog, font') -> do
            GL.glClearColor 0 0 0.4 0
            --mesh' <- Mesh.fromVector StaticDraw vertices indices
            text' <- Text.init font' "Aq"

            return $ Right State
                { program = prog
                --, mesh = mesh'
                , font = font'
                , text = text'
                }

        Left err -> return $ Left err

renderCallback :: Render State ()
renderCallback = do
    state <- getAppStateUnsafe

    GL.glClear GL.GL_COLOR_BUFFER_BIT
    Program.enable (program state)
    Mesh.enable (mesh $ text state)

    Mesh.render Triangles (mesh $ text state)

    Mesh.disable
    Program.disable

teardownCallback :: Render State ()
teardownCallback = do
    state <- getAppStateUnsafe
    --Mesh.delete (mesh state)
    Program.delete (program state)

vertices :: Vector Vertex
vertices =
    fromList $ quadAt (0, 0) 0.25 0.25

indices :: Vector GLuint
indices = fromList [0, 1, 2, 0, 2, 3]

quadAt :: (GLfloat, GLfloat) -> GLfloat -> GLfloat -> [Vertex]
quadAt (leftUpX, leftUpY) width height =
    [ Vertex { position = V3 (leftUpX + width) leftUpY 0 }
    , Vertex { position = V3 leftUpX leftUpY 0 }
    , Vertex { position = V3 leftUpX (leftUpY - height) 0}
    , Vertex { position = V3 (leftUpX + width) (leftUpY - height) 0}
    ]

eitherTwo :: (Either a b, Either a c) -> Either a (b, c)
eitherTwo (e1, e2)
    | isRight e1 && isRight e2 =
        let Right e1' = e1
            Right e2' = e2
        in Right (e1', e2')
    | isLeft e1 =
        let Left err = e1
        in Left err
    | otherwise =
        let Left err = e2
        in Left err
