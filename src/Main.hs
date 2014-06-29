module Main
where

import Graphics.UI.GLUT

import Geometry
import Algorithms.Sampling (visualizeSamplingAlgorithm)
import qualified Algorithms.Sampling.BridsonPoissonDisc as BridsonPoissonDisc
import qualified Algorithms.Sampling.MitchellBestCandidate as MitchellBestCandidate
import qualified Algorithms.Sampling.Uniform as Uniform


bridsonPoissonDiscAlgorithm = BridsonPoissonDisc.BridsonPoissonDiscAlgorithm $
        BridsonPoissonDisc.BridsonPoissonDiscData {
            BridsonPoissonDisc.board = Board2D 200 200,
            BridsonPoissonDisc.numTotalSamples = 400,
            BridsonPoissonDisc.numCandidates = 10,
            BridsonPoissonDisc.discRadius = 10
        }


mitchellBestCandidateAlgorithm = MitchellBestCandidate.MitchellBestCandidateAlgorithm $
    MitchellBestCandidate.MitchellBestCandidateData {
        MitchellBestCandidate.board = Board2D 200 200,
        MitchellBestCandidate.numTotalSamples = 400,
        MitchellBestCandidate.numInitialSamples = 10,
        MitchellBestCandidate.numCandidates = 10
    }


uniformAlgorithm = Uniform.UniformAlgorithm $
    Uniform.UniformData {
        Uniform.board = Board2D 200 200,
        Uniform.numSamples = 400
    }



main = do
    (_progName, _args) <- getArgsAndInitialize

    visualizeSamplingAlgorithm $ uniformAlgorithm

    --visualizeSamplingAlgorithm $ mitchellBestCandidateAlgorithm

    --visualizeSamplingAlgorithm $ bridsonPoissonDiscAlgorithm
