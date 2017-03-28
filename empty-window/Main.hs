module Main where

import           Control.Monad.IO.Class (liftIO)
import           Graphics.BigEngine
import qualified Graphics.GL            as GL

import           System.IO              (hFlush, stdout)
import           Text.Printf            (printf)

main :: IO ()
main = do
    let conf = Configuration { versionMajor = 3
                             , versionMinor = 3
                             , displayMode = SizedScreen (1024, 768)
                             , windowCaption = "Empty Window"
                             , preamble = preambleCallback
                             , frame = frameCallback
                             , postamble = postambleCallback
                             }
    res <- runEngine conf 0
    print res

preambleCallback :: Render Int (Either String ())
preambleCallback = do
    GL.glClearColor 0 0 0.4 0
    putAppState 1
    setWindowSizeCallback (Just windowSizeCallback)
    return $ Right ()

frameCallback :: Render Int ()
frameCallback = do
    GL.glClear GL.GL_COLOR_BUFFER_BIT
    modifyAppState (+ 1)

postambleCallback :: Render Int ()
postambleCallback = do
    liftIO $ putStrLn "Postamble"
    state <- getAppState
    liftIO $ print state

windowSizeCallback :: Int -> Int -> Render Int ()
windowSizeCallback width height = do
    (width', height') <- displayDimension
    liftIO $ do
        printf "windowSizeCallback: (%d, %d) (%d, %d)\n" width height width' height'
        hFlush stdout
    setWindowSizeCallback Nothing
