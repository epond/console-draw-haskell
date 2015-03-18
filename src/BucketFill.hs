module BucketFill where

import Common
import Canvas
import qualified Data.Set as Set

bucketFill :: Coordinates -> Char -> Canvas -> ColourLayer
bucketFill origin colour canvas =
    let layerPoints = fillAcc canvas Set.empty [origin]
    in ColourLayer (Set.toList layerPoints) colour

-- https://hackage.haskell.org/package/containers-0.5.5.1/docs/Data-Set.html
fillAcc :: Canvas -> Set.Set Coordinates -> [Coordinates] -> Set.Set Coordinates
fillAcc _ _ _ = Set.empty