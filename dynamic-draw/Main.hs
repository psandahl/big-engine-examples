module Main where

import           BigE.Attribute.Vert_P (Vertex (..))
import           BigE.Math             (toRadians)
import           BigE.Mesh             (Mesh)
import qualified BigE.Mesh             as Mesh
import qualified BigE.Program          as Program
import           BigE.Runtime
import           BigE.Types
import           Data.List             (foldl')
import           Data.Vector.Storable  (Vector)
import qualified Data.Vector.Storable  as Vector
import           Graphics.GL           (GLfloat, GLuint)
import qualified Graphics.GL           as GL
import           Linear                (M44, V3 (..), lookAt, perspective,
                                        (!*!))

data State = State
    { program   :: !Program
    , mvpLoc    :: !Location
    , mesh      :: !Mesh
    , skew      :: !GLfloat
    , direction :: !Direction
    , persp     :: !(M44 GLfloat)
    , view      :: !(M44 GLfloat)
    } deriving Show

data Direction
    = Forward
    | Backward
    deriving (Eq, Show)

main :: IO ()
main = do
    let conf = Configuration { versionMajor = 3
                             , versionMinor = 3
                             , displayMode = SizedScreen (1024, 768)
                             , windowCaption = "Dynamic Render"
                             , setup = setupCallback
                             , animate = animateCallback
                             , render = renderCallback
                             , teardown = teardownCallback
                             }
    result <- runBigE conf
    print result

setupCallback :: Render State (Either String State)
setupCallback = do
    eProgram <- Program.fromFile
                   [ (VertexShader, "dynamic-draw/vertex.glsl")
                   , (FragmentShader, "dynamic-draw/fragment.glsl")
                   ]
    case eProgram of
        Right program' -> do
            let vectors = genVectors 0
            mvpLoc' <- Program.getUniformLocation program' "mvp"
            mesh' <- Mesh.fromVector DynamicDraw (fst vectors) (snd vectors)
            (width, height) <- displayDimensions

            setWindowSizeCallback (Just windowSizeCallback)

            GL.glClearColor 0 0 0.4 0

            return $ Right State
                { program = program'
                , mvpLoc = mvpLoc'
                , mesh = mesh'
                , skew = 0
                , direction = Forward
                , persp = makePerspective width height
                , view = lookAt (V3 1 5 10) (V3 0 3 0) (V3 0 1 0)
                }

        Left err -> return $ Left err

animateCallback :: Render State ()
animateCallback = do
    state <- getAppStateUnsafe
    duration <- realToFrac <$> frameDuration
    let move = duration * range * 0.5
        state' = updateSkew move state

    let vectors = genVectors (skew state')
    mesh' <- Mesh.update (fst vectors) (snd vectors) (mesh state')

    putAppState state' { mesh = mesh' }

updateSkew :: GLfloat -> State -> State
updateSkew move state
    | skew state < right && direction state == Forward =
        state { skew = skew state + move }
    | skew state > left && direction state == Backward =
        state { skew = skew state - move }
    | direction state == Forward = state { direction = Backward }
    | otherwise = state { direction = Forward }

renderCallback :: Render State ()
renderCallback = do
    state <- getAppStateUnsafe

    GL.glClear GL.GL_COLOR_BUFFER_BIT

    Program.enable (program state)
    Mesh.enable (mesh state)

    let mvp = persp state !*! view state
    setUniform (mvpLoc state) mvp
    Mesh.render Triangles (mesh state)

    Mesh.disable
    Program.disable

teardownCallback :: Render State ()
teardownCallback = do
    state <- getAppStateUnsafe
    Mesh.delete (mesh state)
    Program.delete (program state)

windowSizeCallback :: Int -> Int -> Render State ()
windowSizeCallback width height =
    modifyAppState (\state -> state { persp = makePerspective width height })

genVectors :: GLfloat -> (Vector Vertex, Vector GLuint)
genVectors skew' = (vertices 5 skew', indices 5)

vertices :: Int -> GLfloat -> Vector Vertex
vertices n skew' = do
    Vector.generate (2 + 2 * n) $ \index ->
        let y = yFromIndex index
            reduxFactor = n - round y
            reducedSkew = repeatedHalf reduxFactor skew'
        in
            if even index
                then Vertex {position = V3 (sin reducedSkew * y) (cos reducedSkew * y) (-0.5)}
                else Vertex {position = V3 (sin reducedSkew * y) (cos reducedSkew * y) 0.5}
    where
        yFromIndex :: Int -> GLfloat
        yFromIndex 0 = 0
        yFromIndex idx
            | even idx = (fromIntegral idx) / 2
            | otherwise = yFromIndex $ idx - 1

        repeatedHalf :: Int -> GLfloat -> GLfloat
        repeatedHalf times value =
            foldl' (\value' _ -> 0.5 * value') value [0 .. (times - 1)]

indices :: Int -> Vector GLuint
indices squares =
    Vector.fromList $
        concatMap (\idx ->
            let idx' = idx * 2
            in [idx' + 2, idx' + 3, idx' + 1, idx' + 2, idx' + 1, idx']
        )
        [0 .. (fromIntegral squares - 1)]

range :: GLfloat
range = pi / 2

left :: GLfloat
left = -(pi / 4)

right :: GLfloat
right = pi / 4

makePerspective :: Int -> Int -> M44 GLfloat
makePerspective width height =
    perspective (toRadians 45) (fromIntegral width / fromIntegral height) 0.001 1000
