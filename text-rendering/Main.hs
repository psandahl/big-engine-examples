module Main where

import           BigE.Runtime
import           BigE.TextRenderer      (RenderParams (..), TextRenderer)
import qualified BigE.TextRenderer      as TextRenderer
import qualified BigE.TextRenderer.Font as Font
import           BigE.TextRenderer.Text (Text (..))
import qualified BigE.TextRenderer.Text as Text
import           BigE.Util              (eitherTwo)
import           Control.Monad          (when)
import qualified Graphics.GL            as GL
import           Text.Printf            (printf)

data State = State
    { textRenderer :: !TextRenderer
    , text         :: !Text
    , fps          :: !Double
    } deriving Show

main :: IO ()
main = do
    let conf = Configuration { versionMajor = 3
                             , versionMinor = 3
                             , displayMode = SizedScreen (1024, 768)
                             , windowCaption = "Text Rendering"
                             , setup = setupCallback
                             , animate = animateCallback
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

            -- Allocate a text buffer wide enough for updates.
            text' <- Text.init font' "                          "

            return $ Right State
                { textRenderer = textRenderer'
                , text = text'
                , fps = 0
                }

        Left err -> return $ Left err

animateCallback :: Render State ()
animateCallback = do
    state <- getAppStateUnsafe

    duration <- frameDuration
    let newFps = 1.0  / (duration + 0.000001)
        outStr = printf "FPS: %.2f" newFps
    when (abs (newFps - fps state) > 0.5) $ do
        newText <- Text.update outStr (text state)
        putAppState $ state { text = newText, fps = newFps }

renderCallback :: Render State ()
renderCallback = do
    state <- getAppStateUnsafe

    GL.glClear GL.GL_COLOR_BUFFER_BIT
    TextRenderer.render (text state) (RenderParams 25) (textRenderer state)

teardownCallback :: Render State ()
teardownCallback = do
    state <- getAppStateUnsafe
    TextRenderer.delete (textRenderer state)
    Text.delete (text state)
