module Main where

import           BigE.Attribute.Vert_P  (Vertex (..))
import           BigE.Mesh              (Mesh)
import qualified BigE.Mesh              as Mesh
import qualified BigE.Program           as Program
import           BigE.Runtime
import           BigE.TextRenderer.Font (Font)
import qualified BigE.TextRenderer.Font as Font
import           BigE.TextRenderer.Text (Text (mesh))
import qualified BigE.TextRenderer.Text as Text
import           BigE.Texture           (TextureParameters (..),
                                         defaultParams2D)
import qualified BigE.Texture           as Texture
import           BigE.Types
import           Data.Either            (isLeft, isRight)
import           Data.Vector.Storable   (Vector, fromList)
import           Graphics.GL            (GLfloat, GLint, GLuint)
import qualified Graphics.GL            as GL
import           Linear                 (V3 (..))

data State = State
    { program      :: !Program
    , meshh        :: !Mesh
    , font         :: !Font
    , fontAtlasLoc :: !Location
    , fontAtlas    :: !Texture
    , text         :: !Text
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
    eFontAtlas <- Texture.fromFile2D "text-rendering/noto-bold.png"
                defaultParams2D { format = RGBA8
                                , wrapS = WrapClampToEdge
                                , wrapT = WrapClampToEdge
                                }

    case eitherThree (eProg, eFont, eFontAtlas) of
        Right (prog, font', fontAtlas') -> do
            fontAtlasLoc' <- Program.getUniformLocation prog "fontAtlas"

            GL.glClearColor 0 0 0.4 0
            GL.glEnable GL.GL_BLEND
            GL.glBlendFunc GL.GL_SRC_ALPHA GL.GL_ONE_MINUS_SRC_ALPHA

            meshh' <- Mesh.fromVector StaticDraw vertices indices
            text' <- Text.init font' "Haskell OpenGL Rock!"

            return $ Right State
                { program = prog
                , meshh = meshh'
                , font = font'
                , fontAtlas = fontAtlas'
                , fontAtlasLoc = fontAtlasLoc'
                , text = text'
                }

        Left err -> return $ Left err

renderCallback :: Render State ()
renderCallback = do
    state <- getAppStateUnsafe

    GL.glClear GL.GL_COLOR_BUFFER_BIT

    Program.enable (program state)
    Mesh.enable (mesh $ text state)
    Texture.enable2D 0 (fontAtlas state)
    setUniform (fontAtlasLoc state) (0 :: GLint)

    Mesh.render Triangles (mesh $ text state)

    Texture.disable2D 0
    Mesh.disable
    Program.disable

teardownCallback :: Render State ()
teardownCallback = do
    state <- getAppStateUnsafe
    --Mesh.delete (mesh state)
    Program.delete (program state)

vertices :: Vector Vertex
vertices =
    fromList $ quadAt (0, 0) 0.5 0.5

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

eitherThree :: (Either a b, Either a c, Either a d) -> Either a (b, c, d)
eitherThree (e1, e2, e3)
    | isRight e1 && isRight e2 && isRight e3 =
        let Right e1' = e1
            Right e2' = e2
            Right e3' = e3
        in Right (e1', e2', e3')
    | isLeft e1 =
        let Left err = e1
        in Left err
    | isLeft e2 =
        let Left err = e2
        in Left err
    | otherwise =
        let Left err = e3
        in Left err
