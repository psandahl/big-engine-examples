module Main where

import           Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8  as BS
import           Graphics.Big
import qualified Graphics.GL            as GL

data State = State
    { program :: !Program
    } deriving Show

main :: IO ()
main = do
    let conf = Configuration { versionMajor = 3
                             , versionMinor = 3
                             , displayMode = SizedScreen (1024, 768)
                             , windowCaption = "Untransformed Triangle"
                             , setup = setupCallback
                             , eachFrame = frameCallback
                             , teardown = teardownCallback
                             }
    result <- runEngine conf
    print result

setupCallback :: Render State (Either String State)
setupCallback = do
    vs <- liftIO $ BS.readFile "untransformed-triangle/vertex.glsl"
    fs <- liftIO $ BS.readFile "untransformed-triangle/fragment.glsl"
    eProg <-
        fromByteString
            [ (VertexShader, "untransformed-triangle/vertex.glsl", vs)
            , (FragmentShader, "untransformed-triangle/fragment.glsl", fs)
            ]

    case eProg of
        Right prog -> do
            GL.glClearColor 0 0 0.4 0
            return $ Right State { program = prog }

        Left err -> return $ Left err


frameCallback :: Render State ()
frameCallback = do
    GL.glClear GL.GL_COLOR_BUFFER_BIT

teardownCallback :: Render State ()
teardownCallback = do
    state <- getAppStateUnsafe
    delete (program state)
