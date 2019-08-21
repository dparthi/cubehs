module Main where

import Graphics.Gloss
main = display (InWindow "Nice Window" (200, 200) (10, 10)) white (Line coordinates)


coordinates :: [Point]
coordinates = [((-40),40)
                , (40,40)
                , (40,(-40))
                , ((-40),(-40))
                , ((-40),40)
              ]