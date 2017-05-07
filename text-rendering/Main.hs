module Main where

import           BigE.Runtime
import           BigE.TextRenderer      (RenderParams (..), TextRenderer)
import qualified BigE.TextRenderer      as TextRenderer
import qualified BigE.TextRenderer.Font as Font
import           BigE.TextRenderer.Text (Text)
import qualified BigE.TextRenderer.Text as Text
import           BigE.Util              (eitherTwo)
import qualified Graphics.GL            as GL

data State = State
    { textRenderer :: !TextRenderer
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
    eFont <- Font.fromFile "text-rendering/noto-bold.fnt"
    eTextRenderer <- TextRenderer.init

    case eitherTwo (eFont, eTextRenderer) of
        Right (font', textRenderer') -> do
            GL.glClearColor 0 0 0.4 0

            text' <- Text.init font' "Haskell OpenGL Rock!"

            return $ Right State
                { textRenderer = textRenderer'
                , text = text'
                }

        Left err -> return $ Left err

renderCallback :: Render State ()
renderCallback = do
    state <- getAppStateUnsafe

    GL.glClear GL.GL_COLOR_BUFFER_BIT
    TextRenderer.render (text state) (RenderParams 20) (textRenderer state)

teardownCallback :: Render State ()
teardownCallback = do
    state <- getAppStateUnsafe
    TextRenderer.delete (textRenderer state)
    Text.delete (text state)
