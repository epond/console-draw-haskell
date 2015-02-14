module Drawing where

import Command
import Canvas

applyCommand :: CanvasCommand -> Canvas -> Either String Canvas
applyCommand (NewCanvasCommand _ _) _ = Left "Getting there"
applyCommand _ _ = Left "Not Implemented"