module Canvas where

import qualified Command as CO

data Canvas = Canvas{rows :: [[Char]]} deriving Eq

emptyCanvas :: Canvas
emptyCanvas = Canvas([])

emptyPosition :: Char
emptyPosition = ' '

createNewCanvas :: Int -> Int -> Canvas
createNewCanvas w h = Canvas ([edgeRow] ++ (replicate h innerRow) ++ [edgeRow])
    where edgeRow = replicate (w+2) '-'
          innerRow = "|" ++ (replicate w emptyPosition) ++ "|"


canvasHeight :: Canvas -> Int
canvasHeight canvas = if (length . rows) canvas == 0 then 0
                      else (length . rows) canvas - 2

canvasWidth :: Canvas -> Int
canvasWidth canvas = if (length . rows) canvas == 0 then 0
                     else (length . head . rows) canvas - 2

getNode :: Canvas -> CO.Coordinates -> Maybe Char
getNode canvas point = if not (isOutOfBounds canvas point)
                       then Just ((rows canvas !! (CO.row point)) !! (CO.column point))
                       else Nothing

isOutOfBounds :: Canvas -> CO.Coordinates -> Bool
isOutOfBounds canvas point = (CO.column point < 1 || CO.column point > canvasWidth canvas) ||
                             (CO.row point < 1    || CO.row point >    canvasHeight canvas)

plot :: Canvas -> CO.Coordinates -> Canvas
plot canvas _ = canvas -- TODO

instance Show Canvas where
    -- removes the last newline character
    show canvas = reverse . drop 1 . reverse . unlines $ rows canvas

instance Read Canvas where
    readsPrec _ input = [(Canvas (lines input), "")]