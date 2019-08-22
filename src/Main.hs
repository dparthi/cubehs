module Main where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort

-- main = display (InWindow "Nice Window" (200, 200) (10, 10)) white (renderCube [])
main = simulate FullScreen blue 1 model renderCube updateCube
-- main = print initialPointTemplate
-- main = print $ length $ getCube initialCoordinates

type Edge = (Point3D,Point3D)
type Cube = [Edge]
type Point3D = (Float, Float, Float)

data Model = Model { coordinates::[Point3D] }
model = Model {coordinates = initialCoordinates}

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
renderCube _ = Circle 80

updateCube :: ViewPort -> Float -> Model -> Model
updateCube _ _ m = m

initialCoordinates = [((-60),60,(-60))
                      , (60,60,(-60))
                      , (60,(-60),(-60))
                      , ((-60),(-60),(-60))
                      , ((-60),60,60)
                      , (60,60,60)
                      , (60,(-60),60)
                      , ((-60),(-60),60)
                      ]