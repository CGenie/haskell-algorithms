module Algorithms.Sampling.Uniform (uniform2D)
where

import System.Random (randomRIO)

import Geometry


uniform2D :: Board2D -> Int -> IO [Point2D]
uniform2D board 0 = return []
uniform2D board@(Board2D {boardX = bx, boardY = by}) numPoints = do
    xp <- randomRIO (0, bx - 1)
    yp <- randomRIO (0, by - 1)

    ps <- uniform2D board (numPoints - 1)

    return ([Point2D (fromIntegral xp) (fromIntegral yp)] ++ ps)
