module Drawing
( applyCommand,
  drawLayer,
  emptyLayer,
  ColourLayer(..)
) where

import qualified Command as CO
import qualified Canvas as CA

applyCommand :: CO.CanvasCommand -> CA.Canvas -> Either String CA.Canvas
applyCommand (CO.NewCanvasCommand width height) _ = Right $ CA.createNewCanvas width height
applyCommand (CO.DrawLineCommand startPos endPos) canvas = drawLine startPos endPos canvas
applyCommand _ _ = Left "Not Implemented"

drawLayer :: ColourLayer -> CA.Canvas -> Either String CA.Canvas
drawLayer (ColourLayer points colour) canvas
    | exists points (CA.isOutOfBounds canvas) = Left "Out of bounds"
    | otherwise                               = Right $ foldl (CA.plot colour) canvas points

drawLine :: CO.Coordinates -> CO.Coordinates -> CA.Canvas -> Either String CA.Canvas
drawLine startPos endPos canvas =
    let difference = endPos `CO.pointDiff` startPos
    in if (CO.row difference /= 0 && CO.column difference /= 0)
       then Left "Only horizontal and vertical lines are supported"
       else drawLayer (ColourLayer (linePoints difference startPos) lineColour) canvas

exists :: [CO.Coordinates] -> (CO.Coordinates -> Bool) -> Bool
exists [] _ = False
exists (x:xs) f = f x || exists xs f

linePoints :: CO.Coordinates -> CO.Coordinates -> [CO.Coordinates]
linePoints diff startPos =
    if (CO.column diff /= 0)
    then let step = if (CO.column diff > 0) then 1 else -1
         in map (\x -> CO.Coordinates (CO.column startPos + x) (CO.row startPos)) [0,step .. CO.column diff]
    else let step = if (CO.row diff > 0) then 1 else -1
         in map (\x -> CO.Coordinates (CO.column startPos) (CO.row startPos + x)) [0,step .. CO.row diff]

data ColourLayer = ColourLayer [CO.Coordinates] Char deriving Show

emptyLayer :: ColourLayer
emptyLayer = ColourLayer [] CA.emptyPosition

lineColour :: Char
lineColour = 'x'