module Geometry.Point2D
where


data Point2D = Point2D {x :: Int, y :: Int}

(+:+) :: Point2D -> Point2D -> Point2D
(+:+) p1 p2 = Point2D {x = x p1 + x p2, y = y p1 + y p2}

(-:-) :: Point2D -> Point2D -> Point2D
(-:-) p1 p2 = Point2D {x = x p1 - x p2, y = y p1 - y p2}

distance :: Point2D -> Point2D -> Double
distance p1 p2 = sqrt ((fromIntegral (x p2) - fromIntegral (x p1))^2 +
                       (fromIntegral (y p2) - fromIntegral (y p1))^2)
