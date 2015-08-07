{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module KMeans where

import           Data.Default
import           Data.List
import qualified Data.Map     as M

class (Default v, Ord v) => Vector v where
    distance :: v -> v -> Double
    centroid :: [v] -> v

class Vector v => Vectorizable e v where
    toVector :: e -> v

instance Vector (Double, Double) where
    distance (a, b) (c, d) = sqrt $ (c-a)**2 + (d-b)**2
    centroid lst = let (u, v) = foldr (\(a, b) (c, d) -> (a+c, b+d)) (0.0, 0.0) lst
                       n = fromIntegral $ length lst
                   in (u / n, v / n)

instance Vectorizable (Double, Double) (Double, Double) where
    toVector = id

clusterAssignmentPhase :: (Vector v, Vectorizable e v) => [v] -> [e] -> M.Map v [e]
clusterAssignmentPhase centroids points =
    let initialMap = M.fromList $ zip centroids (repeat [])
        in foldr (\p m -> let chosenCentroid = minimumBy (\x y ->
                                               compare (distance x $ toVector p)
                                                       (distance y $ toVector p))
                                                        centroids
                           in M.adjust (p:) chosenCentroid m)
                 initialMap points

newCentroidPhase :: (Vector v, Vectorizable e v) => M.Map м
newCentroidPhase = M.toList . fmap (centroid . map toVector)

{- kMeans :: (Vector v, Vectorizable e v) => (Int -> [e] -> [v]) -> [e] -> [v]
-}
