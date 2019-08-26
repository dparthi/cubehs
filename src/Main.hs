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
-- main = print $ rotateModel model Cube.X $ pi/4
-- main = print $ concat $ map (\(a,b) -> getPath a b) $ getCube $ coordinates $ rotateModel (Model {coordinates = initialCoordinates}) Cube.X $ pi/4
-- main = print $ Cube.getCoordinates $ getCube $ coordinates initModel

type Point3D = (Float, Float, Float)

data Model = Model { cube:: Cube.Cube, lastAxis::Cube.Axis } deriving Show
initModel :: Model
initModel = Model { cube = Cube.getCube initialCoordinates, lastAxis = Cube.X }

eventHandler :: Event -> Model -> Model
eventHandler (EventKey (MouseButton LeftButton) Down xPos yPos) model = model
eventHandler _ model = model

renderCube :: Model -> Picture
renderCube model = mconcat $ map (\x -> Line $ Cube.getLine x) $ cube model

rotateCubeOnAlternateAxes :: ViewPort -> Float -> Model -> Model
rotateCubeOnAlternateAxes _ x model = rotateCubeOnAlternateAxes' x model

rotateCubeOnAlternateAxes' :: Float -> Model -> Model
rotateCubeOnAlternateAxes' _ model 
  | lastAxis model == Cube.X = rotateModel model Cube.Y $ pi/4
  | lastAxis model == Cube.Y = rotateModel model Cube.X $ pi/4
  
rotateModel :: Model -> Cube.Axis -> Float -> Model
rotateModel model axis angle = Model {cube = Cube.rotateCube (cube model) axis angle, lastAxis = axis}

initialCoordinates = [((-120),120,(-120))
                      , (120,120,(-120))
                      , (120,(-120),(-120))
                      , ((-120),(-120),(-120))
                      , ((-120),120,120)
                      , (120,120,120)
                      , (120,(-120),120)
                      , ((-120),(-120),120)
                      ]
