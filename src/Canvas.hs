module Canvas where

-- In Scala this contained rows: Vector[Vector[Char]]
-- see https://www.haskell.org/tutorial/arrays.html
data Canvas = Canvas deriving Show

emptyCanvas :: Canvas
emptyCanvas = Canvas