module Drawing where

import qualified Command as CO
import qualified Canvas as CA

applyCommand :: CO.CanvasCommand -> CA.Canvas -> Either String CA.Canvas
applyCommand (CO.NewCanvasCommand width height) _ = Right $ CA.createNewCanvas width height
applyCommand (CO.DrawLineCommand startPos endPos) canvas = drawLine startPos endPos canvas
applyCommand _ _ = Left "Not Implemented"

drawLine :: CO.Coordinates -> CO.Coordinates -> CA.Canvas -> Either String CA.Canvas
drawLine _ _ _ = Right CA.emptyCanvas -- TODO