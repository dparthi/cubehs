module Cube where

type Point3D = (Float, Float, Float)
type Edge = (Point3D,Point3D)
type Cube = [Edge]

data Axis = X | Y | Z deriving (Eq, Show)

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

rotateCube :: Cube -> Axis -> Float -> Cube
rotateCube cube axis angle = getCube $ map (\coordinate -> myRotate coordinate axis angle) $ getCoordinates cube

myRotate :: Point3D -> Axis -> Float -> Point3D
myRotate (x, y, z) axis angle
    | axis == X = (x, (y * cos angle) - (z * sin angle), (y * sin angle) + (z * cos angle))
    | axis == Y = ((x * cos angle) + (z * sin angle), y, (z * cos angle) - (x * sin angle))
    | axis == Z = ((x * cos angle) - (y * sin angle), (x * sin angle) + (y * cos angle), z)

getCoordinates :: Cube -> [Point3D]
getCoordinates cube = [p1,p2,p3,p4,p5,p6,p7,p8]
  where p1 = fst $ cube !! 0
        p2 = fst $ cube !! 1
        p3 = fst $ cube !! 2
        p4 = fst $ cube !! 3
        p5 = fst $ cube !! 5
        p6 = fst $ cube !! 6
        p7 = fst $ cube !! 7
        p8 = fst $ cube !! 8