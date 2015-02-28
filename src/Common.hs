module Common where

data ColourLayer = ColourLayer [Coordinates] Char deriving Show

emptyLayer :: ColourLayer
emptyLayer = ColourLayer [] emptyPosition

lineColour :: Char
lineColour = 'x'

emptyPosition :: Char
emptyPosition = ' '

data Coordinates = Coordinates{column :: Int, row :: Int} deriving (Show, Eq)

-- idea from http://stackoverflow.com/a/17768051
instance Num Coordinates where
   Coordinates a b + Coordinates c d = Coordinates (a+c) (b+d)
   Coordinates a b * Coordinates c d = Coordinates (a*c) (b*d)
   Coordinates a b - Coordinates c d = Coordinates (a-c) (b-d)
   abs (Coordinates a b) = Coordinates (abs a) (abs b)
   signum (Coordinates a b) = Coordinates (signum a) (signum b)
   fromInteger i = Coordinates (fromInteger i) (fromInteger i)

-- Find the difference between two points
pointDiff :: Coordinates -> Coordinates -> Coordinates
pointDiff this that = Coordinates (column this - column that) (row this - row that)