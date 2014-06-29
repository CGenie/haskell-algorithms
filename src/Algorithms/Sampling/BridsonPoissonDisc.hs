module Algorithms.Sampling.BridsonPoissonDisc
where

import Control.Monad
import System.Random (randomIO, randomRIO)

import Algorithms.Sampling.Uniform (uniform2D)
import Geometry


data Samples = Samples {active :: [Point2D], inactive :: [Point2D]}


tailSafe :: [a] -> [a]
tailSafe [] = []
tailSafe (x:xs) = xs


totalSamples :: Samples -> [Point2D]
totalSamples (Samples {active = active, inactive = inactive}) = active ++ inactive


findCandidatesInAnnulus :: Board2D -> Annulus2D -> Int -> IO [Point2D]
findCandidatesInAnnulus _ _ 0 = return []
findCandidatesInAnnulus board
                     annulus@(Annulus2D {annulusCenter = p, radiusm = rm, radiusM = rM})
                     numCandidates = do
    phiBase <- randomIO :: IO Double
    rBase <- randomIO :: IO Double

    let r = rm + (rM - rm)*rBase
    let phi = 2*pi*phiBase;

    remaining <- findCandidatesInAnnulus board annulus (numCandidates - 1)

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
bridsonPoissonDiscFindNew board
                          samples@(Samples {active = activeSamples, inactive = _})
                          numCandidates
                          discRadius = do
    rInt <- randomRIO (0, (length activeSamples) - 1)
    let rSample = activeSamples !! rInt
    let annulus = Annulus2D {annulusCenter = rSample, radiusm = discRadius, radiusM = 2.0*discRadius}

    candidates <- findCandidatesInAnnulus board annulus numCandidates
    let outside = filter (\p -> distanceFromSet p (totalSamples samples) >= discRadius) candidates

    return $ sampleWithOutsideCalculate samples outside rInt


bridsonPoissonDisc :: Board2D -> Int -> Int -> Double -> IO [Point2D]
bridsonPoissonDisc board numTotalSamples numCandidates discRadius = do
    activeSamples <- uniform2D board 1
    let inactiveSamples = []

    let samples = Samples {active = activeSamples, inactive = inactiveSamples}

    computedSamples <- foldM bridsonPoissonDiscFindNew' samples [0..numTotalSamples]

    return $ totalSamples computedSamples

    where
        bridsonPoissonDiscFindNew' :: Samples -> Int -> IO Samples
        bridsonPoissonDiscFindNew' s i = bridsonPoissonDiscFindNew board s numCandidates discRadius
