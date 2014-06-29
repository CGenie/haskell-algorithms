module Algorithms.Sampling.MitchellBestCandidate
where

import Control.Monad
import System.Random (randomRIO)

import Algorithms.Sampling.Uniform (uniform2D)
import Geometry


mitchellFindNewSample :: Board2D -> Int -> [Point2D] -> IO [Point2D]
mitchellFindNewSample board numCandidates samples = do
    candidates <- uniform2D board numCandidates
    return $ samples ++ [farthestPoint candidates samples]


mitchellBestCandidate :: Board2D -> Int -> Int -> Int -> IO [Point2D]
mitchellBestCandidate board numTotalSamples numInitialSamples numCandidates = do
    samples <- uniform2D board numInitialSamples
    foldM mitchellFindNewSample' samples [0..(numTotalSamples - numInitialSamples)]
    where
        mitchellFindNewSample' samples i = mitchellFindNewSample board numCandidates samples
