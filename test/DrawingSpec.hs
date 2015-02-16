{-# LANGUAGE OverloadedStrings #-}

module DrawingSpec where

import Test.Hspec
import Drawing
import Canvas
import Command

blank20by4Canvas = read "\
\----------------------\n\
\|                    |\n\
\|                    |\n\
\|                    |\n\
\|                    |\n\
\----------------------" :: Canvas

spec :: Spec
spec = do
    describe "drawLayer" $ do
        it "Given an empty ColourLayer and empty Canvas then return the empty Canvas" $ do
            drawLayer emptyLayer emptyCanvas `shouldBe` Right emptyCanvas
        it "Given an empty ColourLayer and blank Canvas then return the blank Canvas" $ do
            drawLayer emptyLayer blank20by4Canvas `shouldBe` Right blank20by4Canvas
        it "Given a non-empty ColourLayer and blank Canvas then return the expected Canvas" $ do
            let layer = ColourLayer [Coordinates 1 1, Coordinates 2 2] 'x'
            let expectedCanvas = read "\
\----------------------\n\
\|x                   |\n\
\| x                  |\n\
\|                    |\n\
\|                    |\n\
\----------------------" :: Canvas
            drawLayer layer blank20by4Canvas `shouldBe` Right expectedCanvas
        it "Given a ColourLayer out of the Canvas bounds then return an error" $ do
            drawLayer (ColourLayer [Coordinates 4 1] 'x') blank20by4Canvas `shouldBe` Left "Out of bounds"
            drawLayer (ColourLayer [Coordinates 3 2, Coordinates 3 3] 'x') blank20by4Canvas `shouldBe` Left "Out of bounds"
            drawLayer (ColourLayer [Coordinates 3 3] 'x') blank20by4Canvas `shouldBe` Left "Out of bounds"
    describe "applyCommand" $ do
        it "Given a NewCanvas command then return a blank canvas" $ do
            applyCommand (NewCanvasCommand 20 4) emptyCanvas `shouldBe` Right blank20by4Canvas
        it "Given a DrawLine command on a blank canvas then the Canvas should contain the line" $ do
            let expectedCanvas = read "\
\----------------------\n\
\|                    |\n\
\|xxxxxx              |\n\
\|                    |\n\
\|                    |\n\
\----------------------" :: Canvas
            let command = DrawLineCommand (Coordinates 1 2) (Coordinates 6 2)
            applyCommand command blank20by4Canvas `shouldBe` Right expectedCanvas
