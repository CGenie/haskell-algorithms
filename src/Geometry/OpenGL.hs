module Geometry.OpenGL
where

import Geometry.Board2D
import Geometry.Point2D


pointToGL :: Board2D -> Point2D -> (GLfloat, GLfloat, GLfloat)
pointToGL (Board2D bx by) (Point2D px py) = (2.0*(fromIntegral px) / (fromIntegral bx) - 1.0,
                                             2.0*(fromIntegral py) / (fromIntegral by) - 1.0,
                                             0)
