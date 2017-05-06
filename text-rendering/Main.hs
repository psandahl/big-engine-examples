module Main where

import qualified BigE.Mesh              as Mesh
import qualified BigE.Program           as Program
import           BigE.Runtime
import           BigE.TextRenderer.Font (Font)
import qualified BigE.TextRenderer.Font as Font
import           BigE.TextRenderer.Text (Text (mesh))
import qualified BigE.TextRenderer.Text as Text
import           BigE.Types
import           BigE.Util              (eitherTwo)
import           Graphics.GL            (GLint)
import qualified Graphics.GL            as GL

data State = State
    { program      :: !Program
    , font         :: !Font
    , fontAtlasLoc :: !Location
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

    case eitherTwo (eProg, eFont) of
        Right (prog, font') -> do
            fontAtlasLoc' <- Program.getUniformLocation prog "fontAtlas"

            GL.glClearColor 0 0 0.4 0
            GL.glEnable GL.GL_BLEND
            GL.glBlendFunc GL.GL_SRC_ALPHA GL.GL_ONE_MINUS_SRC_ALPHA

            text' <- Text.init font' "Haskell OpenGL Rock!"

            return $ Right State
                { program = prog
                , font = font'
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
    Font.enable 0 (font state)
    setUniform (fontAtlasLoc state) (0 :: GLint)

    Mesh.render Triangles (mesh $ text state)

    Font.disable 0
    Mesh.disable
    Program.disable

teardownCallback :: Render State ()
teardownCallback = do
    state <- getAppStateUnsafe
    Font.delete (font state)
    Program.delete (program state)
