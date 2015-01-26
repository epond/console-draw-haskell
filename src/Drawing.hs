module Drawing where

import Command
import Canvas

applyCommand :: CanvasCommand -> Canvas -> Either String Canvas
applyCommand _ _ = Left "Not Implemented"