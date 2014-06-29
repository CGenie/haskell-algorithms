{-# LANGUAGE NullaryTypeClasses #-}

module Algorithms.Data
where


import Geometry


class Algorithm d where
    algorithmVisualize :: d -> IO [Point2D]
    algorithmWindowTitle :: d -> String

