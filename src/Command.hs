module Command where

import Data.Char
import Common

parseCommand :: String -> Maybe CanvasCommand
parseCommand commandString = case ((words . normaliseCase) commandString) of
    "C" : w : h : [] -> Just $ NewCanvasCommand (read w) (read h)
    "L" : x1 : y1 : x2 : y2 : [] -> Just $ DrawLineCommand (Coordinates (read x1) (read y1)) (Coordinates (read x2) (read y2))
    "R" : x1 : y1 : x2 : y2 : [] -> Just $ DrawRectangleCommand (Coordinates (read x1) (read y1)) (Coordinates (read x2) (read y2))
    "B" : x : y : c : [] -> Just $ BucketFillCommand (Coordinates (read x) (read y)) (head c)
    "CLR" : [] -> Just ClearCommand
    _ -> Nothing

normaliseCase :: String -> String
normaliseCase [] = []
normaliseCase (' ':rest) = ' ' : rest
normaliseCase (x:xs)     = (toUpper x) : normaliseCase xs

data CanvasCommand =
    NewCanvasCommand{width :: Int, height :: Int} |
    DrawLineCommand{startPos :: Coordinates, endPos :: Coordinates} |
    DrawRectangleCommand{ulCorner :: Coordinates, lrCorner :: Coordinates} |
    BucketFillCommand{origin :: Coordinates, colour :: Char} |
    ClearCommand deriving (Show, Eq)
