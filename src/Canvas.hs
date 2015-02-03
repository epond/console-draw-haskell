module Canvas where

data Canvas = Canvas{rows :: [[Int]]} deriving Show

emptyCanvas :: Canvas
emptyCanvas = Canvas([])