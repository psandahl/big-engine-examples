module Main where

import           Control.Monad.IO.Class (liftIO)
import           Graphics.Big
import qualified Graphics.GL            as GL

import           System.IO              (hFlush, stdout)
import           Text.Printf            (printf)

main :: IO ()
main = do
    let conf = Configuration { versionMajor = 3
                             , versionMinor = 3
                             , displayMode = SizedScreen (1024, 768)
                             , windowCaption = "Empty Window"
                             , setup = setupCallback
                             , animate = return ()
                             , render = renderCallback
                             , teardown = teardownCallback
                             }
    res <- runEngine conf
    print res

setupCallback :: Render Int (Either String Int)
setupCallback = do
    liftIO $ putStrLn "setup"
    GL.glClearColor 0 0 0.4 0
    setWindowSizeCallback (Just windowSizeCallback)
    return $ Right 1

renderCallback :: Render Int ()
renderCallback = do
    GL.glClear GL.GL_COLOR_BUFFER_BIT
    modifyAppState (+ 1)

teardownCallback :: Render Int ()
teardownCallback = do
    liftIO $ putStrLn "teardown"
    state <- getAppStateUnsafe
    liftIO $ print state

windowSizeCallback :: Int -> Int -> Render Int ()
windowSizeCallback width height = do
    (width', height') <- displayDimensions
    liftIO $ do
        printf "windowSizeCallback: (%d, %d) (%d, %d)\n" width height width' height'
        hFlush stdout
    setWindowSizeCallback Nothing
