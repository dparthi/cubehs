module Main where

import Graphics.Gloss
main = display (InWindow "Nice Window" (200, 200) (10, 10)) white (renderCube [])
-- main = print initialPointTemplate

type Edge = [Point]
type Cube = [Edge]

p1,p2,p3,p4,p5,p6,p7,p8 :: Point
p1 = ((-1),1)
p2 = (1,1)
p3 = (1,(-1))
p4 = ((-1),(-1))
p5 = ((-1),1)
p6 = (1,1)
p7 = (1,(-1))
p8 = ((-1),(-1))

initialPoints :: [Point]
initialPoints = [p1,p2,p3,p4,p5,p6,p7,p8]

initialEdges :: [Edge]
initialEdges = [] 

renderCube :: Cube -> Picture
renderCube _ = Circle 80