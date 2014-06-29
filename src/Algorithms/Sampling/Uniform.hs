module Algorithms.Sampling.Uniform
where

import System.Random (randomRIO)

import Algorithms.Data
import Geometry


data UniformData = UniformData {
    board :: Board2D,
    numSamples :: Int
}
data UniformAlgorithm = UniformAlgorithm UniformData
instance Algorithm UniformAlgorithm where
    algorithmVisualize (UniformAlgorithm d) = uniform2D d
    algorithmWindowTitle _ = "Uniform 2D"



uniform2D :: UniformData -> IO [Point2D]
uniform2D (UniformData {board = _, numSamples = 0}) = return []
uniform2D d = do
    xp <- randomRIO (0, (boardX $ board d) - 1)
    yp <- randomRIO (0, (boardY $ board d) - 1)

    ps <- uniform2D (d {numSamples = (numSamples d) - 1})

    return ([Point2D (fromIntegral xp) (fromIntegral yp)] ++ ps)
