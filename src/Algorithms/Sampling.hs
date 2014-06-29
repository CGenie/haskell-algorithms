-- Some algorithms here are based on the great presentation
-- http://bost.ocks.org/mike/algorithms/

module Algorithms.Sampling (
    mitchellBestCandidate,
    uniform2D,
    visualizeSamplingAlgorithm)
where

import Graphics.UI.GLUT

import Algorithms.Data
import Algorithms.Sampling.MitchellBestCandidate
import Algorithms.Sampling.Uniform


import Geometry


displayPointsCallback :: Board2D -> [Point2D] -> DisplayCallback
displayPointsCallback b pts = do
    clear [ColorBuffer]

    let circles = map (\p -> Circle2D p 1) pts

    mapM_ (drawCircle b) circles

    flush


visualizeSamplingAlgorithm :: (Algorithm d) => d -> IO ()
visualizeSamplingAlgorithm algorithmData = do

    _window <- createWindow $ algorithmWindowTitle algorithmData

    let b = Board2D 200 200
    pts <- algorithmVisualize algorithmData

    displayCallback $= displayPointsCallback b pts
    
    mainLoop
