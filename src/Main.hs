-- rendering cube is working
-- rotating cube is working
-- handling events is TBD

module Main where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.IO.Interact

import qualified Cube as Cube
-- main = display (InWindow "Nice Window" (200, 200) (10, 10)) white (renderCube [])
-- main = simulate FullScreen blue 1 initModel renderCube rotateCubeOnAlternateAxes
main = play FullScreen blue 1 initModel renderCube eventHandler rotateCubeOnAlternateAxes'
-- main = print initialPointTemplate
-- main = print $ getCube $ coordinates model
-- main = print $ rotateModel model X $ pi/4
-- main = print $ concat $ map (\(a,b) -> getPath a b) $ getCube $ coordinates $ rotateModel (Model {coordinates = initialCoordinates}) X $ pi/4
-- main = print $ Cube.getCoordinates $ getCube $ coordinates initModel

type Point3D = (Float, Float, Float)

data Axis = X | Y | Z deriving (Eq, Show)
data Model = Model { cube:: Cube.Cube, lastAxis::Axis } deriving Show
initModel :: Model
initModel = Model { cube = Cube.getCube initialCoordinates, lastAxis = X }

eventHandler :: Event -> Model -> Model
eventHandler (EventKey (MouseButton LeftButton) Down xPos yPos) model = model
eventHandler _ model = model

renderCube :: Model -> Picture
renderCube model = mconcat $ map (\(a,b) -> Line $ getPath a b) $ cube model

getPath :: Point3D -> Point3D -> Path
getPath (x1,y1,_) (x2,y2,_) = [(x1,y1),(x2,y2)]

-- | This alternate choice of axes is just to simulate and test the rotational behaviour. Perhaps this function can be named better! For now naming it as rotateCubeOnAlternateAxes
rotateCubeOnAlternateAxes :: ViewPort -> Float -> Model -> Model
rotateCubeOnAlternateAxes _ _ model
  | lastAxis model == X = rotateModel model Y $ pi/4
  | lastAxis model == Y = rotateModel model X $ pi/4

rotateCubeOnAlternateAxes' :: Float -> Model -> Model
rotateCubeOnAlternateAxes' _ model
  | lastAxis model == X = rotateModel model Y $ pi/4
  | lastAxis model == Y = rotateModel model X $ pi/4
  
rotateModel :: Model -> Axis -> Float -> Model
rotateModel model axis angle = Model {coordinates = map (\coordinate -> myRotate coordinate axis angle) $ coordinates model, lastAxis = axis}

myRotate :: Point3D -> Axis -> Float -> Point3D
myRotate (x, y, z) axis angle
    | axis == X = (x, (y * cos angle) - (z * sin angle), (y * sin angle) + (z * cos angle))
    | axis == Y = ((x * cos angle) + (z * sin angle), y, (z * cos angle) - (x * sin angle))
    | axis == Z = ((x * cos angle) - (y * sin angle), (x * sin angle) + (y * cos angle), z)

initialCoordinates = [((-120),120,(-120))
                      , (120,120,(-120))
                      , (120,(-120),(-120))
                      , ((-120),(-120),(-120))
                      , ((-120),120,120)
                      , (120,120,120)
                      , (120,(-120),120)
                      , ((-120),(-120),120)
                      ]
