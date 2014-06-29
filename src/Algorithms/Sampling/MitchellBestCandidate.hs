module Algorithms.Sampling.MitchellBestCandidate
where

import Control.Monad

import Algorithms.Data
import qualified Algorithms.Sampling.Uniform as Uniform
import Geometry


data MitchellBestCandidateData = MitchellBestCandidateData {
    board :: Board2D,
    numTotalSamples :: Int,
    numInitialSamples :: Int,
    numCandidates :: Int
}
data MitchellBestCandidateAlgorithm = MitchellBestCandidateAlgorithm MitchellBestCandidateData
instance Algorithm MitchellBestCandidateAlgorithm where
    algorithmVisualize (MitchellBestCandidateAlgorithm d) = mitchellBestCandidate d
    algorithmWindowTitle _ = "Mitchell's Best Candidate"



mitchellFindNewSample :: Board2D -> Int -> [Point2D] -> IO [Point2D]
mitchellFindNewSample b nc s = do
    candidates <- Uniform.uniform2D (Uniform.UniformData {Uniform.board = b,
                                                          Uniform.numSamples = nc})
    return $ s ++ [farthestPoint candidates s]


mitchellBestCandidate :: MitchellBestCandidateData -> IO [Point2D]
mitchellBestCandidate d = do
    samples <- Uniform.uniform2D (Uniform.UniformData {Uniform.board = board d,
                                                       Uniform.numSamples = numInitialSamples d})
    foldM mitchellFindNewSample' samples [0..((numTotalSamples d) - (numInitialSamples d))]
    where
        mitchellFindNewSample' samples _ = mitchellFindNewSample (board d) (numCandidates d) samples
