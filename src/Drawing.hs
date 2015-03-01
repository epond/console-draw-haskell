module Drawing
( applyCommand,
  drawLayer,
  emptyLayer,
  ColourLayer(..)
) where

import Common
import Command
import Canvas
import BucketFill

applyCommand :: CanvasCommand -> Canvas -> Either String Canvas
applyCommand (NewCanvasCommand w h) _ = Right $ createNewCanvas w h
applyCommand (DrawLineCommand start end) canvas = drawLine start end canvas
applyCommand (DrawRectangleCommand ul lr) canvas =
    Right canvas >>=
        drawLine (Coordinates (column ul) (row ul)) (Coordinates (column lr) (row ul)) >>=
        drawLine (Coordinates (column lr) (row ul)) (Coordinates (column lr) (row lr)) >>=
        drawLine (Coordinates (column lr) (row lr)) (Coordinates (column ul) (row lr)) >>=
        drawLine (Coordinates (column ul) (row lr)) (Coordinates (column ul) (row ul))
applyCommand (BucketFillCommand fillOrigin fillColour) canvas = drawLayer (bucketFill fillOrigin fillColour canvas) canvas
applyCommand ClearCommand canvas       = Right $ createNewCanvas (canvasWidth canvas) (canvasHeight canvas)

drawLayer :: ColourLayer -> Canvas -> Either String Canvas
drawLayer (ColourLayer points drawColour) canvas
    | exists points (isOutOfBounds canvas) = Left "Out of bounds"
    | otherwise                            = Right $ foldl (plot drawColour) canvas points

drawLine :: Coordinates -> Coordinates -> Canvas -> Either String Canvas
drawLine start end canvas =
    let difference = end `pointDiff` start
    in if (row difference /= 0 && column difference /= 0)
       then Left "Only horizontal and vertical lines are supported"
       else let points = linePoints difference start
                lineLayer = ColourLayer points lineColour
            in drawLayer lineLayer canvas

exists :: [Coordinates] -> (Coordinates -> Bool) -> Bool
exists [] _ = False
exists (x:xs) f = f x || exists xs f

linePoints :: Coordinates -> Coordinates -> [Coordinates]
linePoints diff start =
    if (column diff /= 0)
    then let step = if (column diff > 0) then 1 else -1
         in map (\x -> Coordinates (column start + x) (row start)) [0,step .. column diff]
    else let step = if (row diff > 0) then 1 else -1
         in map (\x -> Coordinates (column start) (row start + x)) [0,step .. row diff]