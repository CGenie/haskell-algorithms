module Geometry
where

import GHC.Exts (sortWith)

import Graphics.UI.GLUT


data Board2D = Board2D {boardX :: Int, boardY :: Int}


data Point2D = Point2D {x :: Double, y :: Double}

(+:+) :: Point2D -> Point2D -> Point2D
(+:+) p1 p2 = Point2D {x = x p1 + x p2, y = y p1 + y p2}

(-:-) :: Point2D -> Point2D -> Point2D
(-:-) p1 p2 = Point2D {x = x p1 - x p2, y = y p1 - y p2}

distance :: Point2D -> Point2D -> Double
distance p1 p2 = sqrt ((x p2 - x p1)^2 + (y p2 - y p1)^2)

-- compute minimal distance between point and set of points
distanceFromSet :: Point2D -> [Point2D] -> Double
distanceFromSet p [] = 1000 -- far away
distanceFromSet p (pt:pts) = min (distance p pt) (distanceFromSet p pts)

-- from a set of points (candidates), find the one that is the
-- farthest from another set of points (sample)
farthestPoint :: [Point2D] -> [Point2D] -> Point2D
farthestPoint candidates sample = farthest
    where
        candidateDistances = map (\p -> (distanceFromSet p sample, p)) candidates
        sortedDistances = sortWith fst candidateDistances
        farthest = snd $ head $ reverse sortedDistances


data Circle2D = Circle2D {circleCenter :: Point2D, radius :: Double}

data Annulus2D = Annulus2D {annulusCenter :: Point2D, radiusm :: Double, radiusM :: Double}

-- openGL functionality

doubleToGLfloat :: Double -> GLfloat
doubleToGLfloat val = fromRational $ toRational val

-- | normalize to (-1, 1)x(-1, 1) range
pointToGL :: Board2D -> Point2D -> (GLfloat, GLfloat, GLfloat)
pointToGL (Board2D bx by) (Point2D px py) = (doubleToGLfloat $ 2.0*px / (fromIntegral bx) - 1.0,
                                             doubleToGLfloat $ 2.0*py / (fromIntegral by) - 1.0,
                                             0)

glPointCircleVertices :: Board2D -> Circle2D -> [Vertex3 GLfloat]
glPointCircleVertices board (Circle2D {circleCenter = p, radius = r}) = vertices
    where
        numSides = 12 :: Int
        circlePointAngle :: Int -> Double
        circlePointAngle i = 2*pi*(fromIntegral i)/(fromIntegral numSides)
        circlePoint :: Int -> Point2D
        circlePoint i = (Point2D {x = r * cos (circlePointAngle i),
                                  y = r * sin (circlePointAngle i)})
                         +:+ p
        circlePoints = map circlePoint [0..numSides]
        vertices = map (\(gx, gy, gz) -> Vertex3 gx gy gz) (map (pointToGL board) circlePoints)
    
drawCircle :: Board2D -> Circle2D -> DisplayCallback
drawCircle board circle = do
    let circleVertices = glPointCircleVertices board circle
    renderPrimitive Polygon $ do
        color $ Color3 (1 :: GLfloat) 0 0
        mapM_ vertex circleVertices
    renderPrimitive LineStrip $ do
         color $ Color3 (1 :: GLfloat) (1 :: GLfloat) (1 :: GLfloat)
         mapM_ vertex circleVertices
