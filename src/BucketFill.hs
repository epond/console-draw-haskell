module BucketFill where

import Common
import Canvas
import qualified Data.Set as Set
import Data.Maybe

bucketFill :: Coordinates -> Char -> Canvas -> ColourLayer
bucketFill origin colour canvas =
    let layerPoints = bucketFillIter canvas Set.empty [origin]
    in ColourLayer (Set.toList layerPoints) colour

pushNode :: Canvas -> Set.Set Coordinates -> Coordinates -> [Coordinates] -> [Coordinates]
pushNode canvas layerPoints node nodeStack =
    if isJust (getNode canvas node) && Set.notMember node layerPoints
    then node : nodeStack
    else nodeStack

isEmptyPosition :: Canvas -> Coordinates -> Bool
isEmptyPosition canvas node = case (getNode canvas node) of
    Just n  -> n == emptyPosition
    Nothing -> False

bucketFillIter :: Canvas -> Set.Set Coordinates -> [Coordinates] -> Set.Set Coordinates
bucketFillIter _ layerPoints []             = layerPoints
bucketFillIter canvas layerPoints nodeStack =
    let node        = head nodeStack
        poppedStack = tail nodeStack
        (newPoints, newStack) = if isEmptyPosition canvas node
                                then let push = pushNode canvas layerPoints
                                         tempStack = push (left node) . push (right node) . push (up node) . push (down node) $ poppedStack
                                     in (Set.insert node layerPoints, tempStack)
                                else (layerPoints, poppedStack)
    in bucketFillIter canvas newPoints newStack