module Algorithms.Sampling.BridsonPoissonDisc
where

import Control.Monad
import System.Random (randomIO, randomRIO)

import Algorithms.Data
import qualified Algorithms.Sampling.Uniform as Uniform
import Geometry


data Samples = Samples {active :: [Point2D], inactive :: [Point2D]}


data BridsonPoissonDiscData = BridsonPoissonDiscData {
    board :: Board2D,
    numTotalSamples :: Int,
    numCandidates :: Int,
    discRadius :: Double
}
data BridsonPoissonDiscAlgorithm = BridsonPoissonDiscAlgorithm BridsonPoissonDiscData
instance Algorithm BridsonPoissonDiscAlgorithm where
    algorithmVisualize (BridsonPoissonDiscAlgorithm d) = bridsonPoissonDisc d
    algorithmWindowTitle _ = "Bridson Poisson-disc"


tailSafe :: [a] -> [a]
tailSafe [] = []
tailSafe (_:xs) = xs


totalSamples :: Samples -> [Point2D]
totalSamples (Samples {active = a, inactive = i}) = a ++ i


findCandidatesInAnnulus :: Board2D -> Annulus2D -> Int -> IO [Point2D]
findCandidatesInAnnulus _ _ 0 = return []
findCandidatesInAnnulus b
                     annulus@(Annulus2D {annulusCenter = p, radiusm = rm, radiusM = rM})
                     nc = do
    phiBase <- randomIO :: IO Double
    rBase <- randomIO :: IO Double

    let r = rm + (rM - rm)*rBase
    let phi = 2*pi*phiBase;

    remaining <- findCandidatesInAnnulus b annulus (nc - 1)

    return $ [Point2D {
        x = r*cos phi,
        y = r*sin phi
    } +:+ p] ++ remaining


sampleWithOutsideCalculate :: Samples -> [Point2D] -> Int -> Samples
sampleWithOutsideCalculate Samples {active = activeSamples, inactive = inactiveSamples}
                           []
                           numActiveSample =
                       Samples {
                               active = na ++ (tailSafe nb),
                               inactive = inactiveSamples ++ [activeSamples !! numActiveSample]
                       }
                       where
                           (na, nb) = splitAt numActiveSample activeSamples

sampleWithOutsideCalculate Samples {active = activeSamples, inactive = inactiveSamples}
                           outside
                           _ =
                       Samples {
                               active = activeSamples ++ [head outside],
                               inactive = inactiveSamples
                       }


bridsonPoissonDiscFindNew :: Board2D -> Samples -> Int -> Double -> IO Samples
bridsonPoissonDiscFindNew _
                          samples@(Samples {active = [], inactive = _})
                          _
                          _ = return samples
bridsonPoissonDiscFindNew b
                          samples@(Samples {active = activeSamples, inactive = _})
                          nc
                          dr = do
    rInt <- randomRIO (0, (length activeSamples) - 1)
    let rSample = activeSamples !! rInt
    let annulus = Annulus2D {annulusCenter = rSample, radiusm = dr, radiusM = 2.0*dr}

    candidates <- findCandidatesInAnnulus b annulus nc
    let outside = filter (\p -> distanceFromSet p (totalSamples samples) >= dr) candidates

    return $ sampleWithOutsideCalculate samples outside rInt


bridsonPoissonDisc :: BridsonPoissonDiscData -> IO [Point2D]
bridsonPoissonDisc d = do
    activeSamples <- Uniform.uniform2D (Uniform.UniformData {Uniform.board = (board d),
                                                             Uniform.numSamples = 1})
    let inactiveSamples = []

    let samples = Samples {active = activeSamples, inactive = inactiveSamples}

    computedSamples <- foldM bridsonPoissonDiscFindNew' samples [0..(numTotalSamples d)]

    return $ totalSamples computedSamples

    where
        bridsonPoissonDiscFindNew' :: Samples -> Int -> IO Samples
        bridsonPoissonDiscFindNew' s _ = bridsonPoissonDiscFindNew (board d)
                                                                   s
                                                                   (numCandidates d)
                                                                   (discRadius d)
