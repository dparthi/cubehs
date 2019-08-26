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

data Model = Model { cube::Cube.Cube, step_angle::Float } deriving Show
initModel :: Model
initModel = Model { cube = Cube.getCube initialCoordinates, step_angle = 0.1 }

step :: Float -> Model -> Model
step _ model = model

renderModel :: Model -> Picture
renderModel model = mconcat $ map (\x -> Line $ Cube.getLine x) $ cube model

rotateModel :: Model -> Cube.Axis -> Float -> Model
rotateModel model axis angle = Model {cube = Cube.rotateCube (cube model) axis angle, step_angle = step_angle model}

eventHandler :: Event -> Model -> Model
eventHandler (EventKey (MouseButton LeftButton) Down xPos yPos) model = model
eventHandler (EventKey (Char 'a') _ _ _) model = Model {cube = Cube.rotateCube (cube model) Cube.Y ((-1) * step_angle model), step_angle = step_angle model}
eventHandler (EventKey (Char 'd') _ _ _) model = Model {cube = Cube.rotateCube (cube model) Cube.Y (step_angle model), step_angle = step_angle model}
eventHandler (EventKey (Char 's') _ _ _) model = Model {cube = Cube.rotateCube (cube model) Cube.X (step_angle model), step_angle = step_angle model}
eventHandler (EventKey (Char 'w') _ _ _) model = Model {cube = Cube.rotateCube (cube model) Cube.X ((-1) * step_angle model), step_angle = step_angle model}
eventHandler (EventKey (SpecialKey KeyLeft) _ _ _) model = Model {cube = Cube.rotateCube (cube model) Cube.Y ((-1) * step_angle model), step_angle = step_angle model}
eventHandler (EventKey (SpecialKey KeyRight) _ _ _) model = Model {cube = Cube.rotateCube (cube model) Cube.Y (step_angle model), step_angle = step_angle model}
eventHandler (EventKey (SpecialKey KeyDown) _ _ _) model = Model {cube = Cube.rotateCube (cube model) Cube.X (step_angle model), step_angle = step_angle model}
eventHandler (EventKey (SpecialKey KeyUp) _ _ _) model = Model {cube = Cube.rotateCube (cube model) Cube.X ((-1) * step_angle model), step_angle = step_angle model}
eventHandler (EventKey (Char '+') _ _ _) model
  | step_angle model < pi = Model {cube = cube model, step_angle = (step_angle model) + 0.1}
  | otherwise = Model {cube = cube model, step_angle = 0.1}
eventHandler (EventKey (Char '-') _ _ _) model
  | step_angle model > 0.1 = Model {cube = cube model, step_angle = (step_angle model) - 0.1}
  | otherwise = Model {cube = cube model, step_angle = 0.1}
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