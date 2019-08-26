module Main where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.IO.Interact

-- main = display (InWindow "Nice Window" (200, 200) (10, 10)) white (renderCube [])
main = simulate FullScreen blue 1 (Model {coordinates = initialCoordinates}) renderCube updateCube
-- main = play FullScreen blue 1 model renderCube eventHandler updateCube
-- main = print initialPointTemplate
-- main = print $ getCube $ coordinates model
-- main = print $ rotateModel model X $ pi/4
-- main = print $ concat $ map (\(a,b) -> getPath a b) $ getCube $ coordinates $ rotateModel (Model {coordinates = initialCoordinates}) X $ pi/4

type Edge = (Point3D,Point3D)
type Cube = [Edge]
type Point3D = (Float, Float, Float)

data Axis = X | Y | Z deriving (Eq, Show)
data Model = Model { coordinates::[Point3D] } deriving Show

eventHandler :: Event -> Model -> Model
eventHandler _ model = model

getCube :: [Point3D] -> Cube
getCube coordinates = [(c1, c2)
                        , (c2, c3)
                        , (c3, c4)
                        , (c4, c1)
                        , (c1, c5)
                        , (c5, c6)
                        , (c6, c7)
                        , (c7, c8)
                        , (c8, c5)
                        , (c2, c6)
                        , (c3, c7)
                        , (c4, c8)]
        where c1 = coordinates !! 0
              c2 = coordinates !! 1
              c3 = coordinates !! 2
              c4 = coordinates !! 3
              c5 = coordinates !! 4
              c6 = coordinates !! 5
              c7 = coordinates !! 6
              c8 = coordinates !! 7

renderCube :: Model -> Picture
renderCube model = mconcat $ map (\(a,b) -> Line $ getPath a b) $ getCube $ coordinates model

getPath :: Point3D -> Point3D -> Path
getPath (x1,y1,_) (x2,y2,_) = [(x1,y1),(x2,y2)]

updateCube :: ViewPort -> Float -> Model -> Model
updateCube _ _ model = rotateModel model X $ pi/4

rotateModel :: Model -> Axis -> Float -> Model
rotateModel model axis angle = Model {coordinates = map (\coordinate -> myRotate coordinate axis angle) $ coordinates model}

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
