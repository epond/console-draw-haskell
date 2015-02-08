module Command where

parseCommand :: String -> Maybe CanvasCommand
parseCommand _ = Just ClearCommand

data CanvasCommand =
    NewCanvasCommand{width :: Int, height :: Int} |
    DrawLineCommand{startPos :: Coordinates, endPos :: Coordinates} |
    DrawRectangleCommand{ulCorner :: Coordinates, lrCorner :: Coordinates} |
    BucketFillCommand{origin :: Coordinates, colour :: Char} |
    ClearCommand deriving (Show, Eq)

data Coordinates = Coordinates{column :: Int, row :: Int} deriving (Show, Eq)
