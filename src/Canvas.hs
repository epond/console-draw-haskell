module Canvas where

data Canvas = Canvas{rows :: [[Char]]}

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


instance Show Canvas where
    show canvas = reverse . drop 1 . reverse . unlines $ rows canvas

instance Read Canvas where
    readsPrec _ input = [(Canvas (lines input), "")]