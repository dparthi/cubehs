-- rendering cube is working
-- rotating cube is working
-- handling events is TBD

module Main where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.IO.Interact

import qualified Cube as Cube
-- main = display (InWindow "Nice Window" (200, 200) (10, 10)) white (renderModel [])
-- main = simulate FullScreen blue 1 initModel renderModel rotateCubeOnAlternateAxes
main = play FullScreen blue 1 initModel renderModel eventHandler step

data Model = Model { cube:: Cube.Cube } deriving Show
initModel :: Model
initModel = Model { cube = Cube.getCube initialCoordinates}

step :: Float -> Model -> Model
step _ model = model

renderModel :: Model -> Picture
renderModel model = mconcat $ map (\x -> Line $ Cube.getLine x) $ cube model

rotateModel :: Model -> Cube.Axis -> Float -> Model
rotateModel model axis angle = Model {cube = Cube.rotateCube (cube model) axis angle}

step_angle :: Float
step_angle = 0.1

eventHandler :: Event -> Model -> Model
eventHandler (EventKey (MouseButton LeftButton) Down xPos yPos) model = model
eventHandler (EventKey (Char 'a') _ _ _) model = Model {cube = Cube.rotateCube (cube model) Cube.Y ((-1) * step_angle)}
eventHandler (EventKey (Char 'd') _ _ _) model = Model {cube = Cube.rotateCube (cube model) Cube.Y step_angle}
eventHandler (EventKey (Char 's') _ _ _) model = Model {cube = Cube.rotateCube (cube model) Cube.X step_angle}
eventHandler (EventKey (Char 'w') _ _ _) model = Model {cube = Cube.rotateCube (cube model) Cube.X ((-1) * step_angle)}
eventHandler _ model = model

initialCoordinates = [((-120),120,(-120))
                      , (120,120,(-120))
                      , (120,(-120),(-120))
                      , ((-120),(-120),(-120))
                      , ((-120),120,120)
                      , (120,120,120)
                      , (120,(-120),120)
                      , ((-120),(-120),120)
                      ]