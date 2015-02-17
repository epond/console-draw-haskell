module Drawing where

import qualified Command as CO
import qualified Canvas as CA

applyCommand :: CO.CanvasCommand -> CA.Canvas -> Either String CA.Canvas
applyCommand (CO.NewCanvasCommand width height) _ = Right $ CA.createNewCanvas width height
applyCommand (CO.DrawLineCommand startPos endPos) canvas = drawLine startPos endPos canvas
applyCommand _ _ = Left "Not Implemented"

drawLayer :: ColourLayer -> CA.Canvas -> Either String CA.Canvas
drawLayer (ColourLayer points colour) canvas
    | exists points (CA.isOutOfBounds canvas) = Left "Out of bounds"
    | otherwise                               = Right $ foldl CA.plot canvas points

exists :: [CO.Coordinates] -> (CO.Coordinates -> Bool) -> Bool
exists [] _ = False
exists (x:xs) f = f x || exists xs f

drawLine :: CO.Coordinates -> CO.Coordinates -> CA.Canvas -> Either String CA.Canvas
drawLine _ _ _ = Right CA.emptyCanvas -- TODO

data ColourLayer = ColourLayer [CO.Coordinates] Char deriving Show

emptyLayer :: ColourLayer
emptyLayer = ColourLayer [] CA.emptyPosition

lineColour :: Char
lineColour = 'x'