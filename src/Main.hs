module Main
where

import Graphics.UI.GLUT

import Geometry
import Algorithms.Sampling (uniform2D, mitchellBestCandidate)


showUniform2D :: IO ()
showUniform2D = do
    _window <- createWindow "Uniform2D"

    let board = Board2D 200 200
    pts <- uniform2D board 400

    displayCallback $= displayPointsCallback board pts
    
    mainLoop


showMitchellBestCandidate :: IO ()
showMitchellBestCandidate = do
    _window <- createWindow "Uniform2D"

    let board = Board2D 200 200
    pts <- mitchellBestCandidate board 400 10 10

    displayCallback $= displayPointsCallback board pts
    
    mainLoop


displayPointsCallback :: Board2D -> [Point2D] -> DisplayCallback
displayPointsCallback board pts = do
    clear [ColorBuffer]

    let circles = map (\p -> Circle2D p 1) pts
    let circleVertices = map (glPointCircleVertices board) circles

    mapM_ (drawCircle board) circles

    flush


main = do
    (_progName, _args) <- getArgsAndInitialize
    showMitchellBestCandidate
