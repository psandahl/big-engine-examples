module Main where

import           Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8  as BS
import           Graphics.Big
import qualified Graphics.GL            as GL

data State = State
    { program :: !(Maybe Program)
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
    result <- runEngine conf $ State { program = Nothing }
    print result

setupCallback :: Render State (Either String ())
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
            modifyAppState $ \state -> state { program = Just prog }

            GL.glClearColor 0 0 0.4 0
            return $ Right ()

        Left err -> return $ Left err


frameCallback :: Render State ()
frameCallback = do
    GL.glClear GL.GL_COLOR_BUFFER_BIT

teardownCallback :: Render State ()
teardownCallback = do
    state <- getAppState
    case program state of
        Just prog -> delete prog
        Nothing   -> return ()
