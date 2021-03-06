{-# LANGUAGE OverloadedStrings #-}

module DrawingSpec where

import Test.Hspec
import Common
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
            drawLayer layer blank20by4Canvas `shouldBe` Right (read "\
                \----------------------\n\
                \|x                   |\n\
                \| x                  |\n\
                \|                    |\n\
                \|                    |\n\
                \----------------------" :: Canvas)
        it "Given a ColourLayer out of the Canvas bounds then return an error" $ do
            drawLayer (ColourLayer [Coordinates 21 1] 'x') blank20by4Canvas `shouldBe` Left "Out of bounds"
            drawLayer (ColourLayer [Coordinates 20 4, Coordinates 20 5] 'x') blank20by4Canvas `shouldBe` Left "Out of bounds"
            drawLayer (ColourLayer [Coordinates 20 5] 'x') blank20by4Canvas `shouldBe` Left "Out of bounds"
    describe "applyCommand" $ do
        it "Given a NewCanvas command then return a blank canvas" $ do
            applyCommand (NewCanvasCommand 20 4) emptyCanvas `shouldBe` Right blank20by4Canvas
        it "Given a DrawLine command on a blank canvas then the Canvas should contain the line" $ do
            let command = DrawLineCommand (Coordinates 1 2) (Coordinates 6 2)
            applyCommand command blank20by4Canvas `shouldBe` Right (read "\
                \----------------------\n\
                \|                    |\n\
                \|xxxxxx              |\n\
                \|                    |\n\
                \|                    |\n\
                \----------------------" :: Canvas)
        it "Given a DrawLine command going left then the Canvas should contain the line" $ do
            let command = DrawLineCommand (Coordinates 6 2) (Coordinates 1 2)
            applyCommand command blank20by4Canvas `shouldBe` Right (read "\
                \----------------------\n\
                \|                    |\n\
                \|xxxxxx              |\n\
                \|                    |\n\
                \|                    |\n\
                \----------------------" :: Canvas)
        it "Given a DrawLine command going up then the Canvas should contain the line" $ do
            let command = DrawLineCommand (Coordinates 2 2) (Coordinates 2 1)
            applyCommand command blank20by4Canvas `shouldBe` Right (read "\
                \----------------------\n\
                \| x                  |\n\
                \| x                  |\n\
                \|                    |\n\
                \|                    |\n\
                \----------------------" :: Canvas)
        it "Given chained commands the canvas should remember them all" $ do
            let canvas =
                    Right emptyCanvas >>=
                    applyCommand (NewCanvasCommand 20 4) >>=
                    applyCommand (DrawLineCommand (Coordinates 1 2) (Coordinates 6 2)) >>=
                    applyCommand (DrawLineCommand (Coordinates 6 3) (Coordinates 6 4))
            canvas `shouldBe` Right (read "\
                \----------------------\n\
                \|                    |\n\
                \|xxxxxx              |\n\
                \|     x              |\n\
                \|     x              |\n\
                \----------------------" :: Canvas)
        it "Given a DrawLine command with an oblique line then an error should be returned" $ do
            let command = DrawLineCommand (Coordinates 1 2) (Coordinates 3 4)
            applyCommand command blank20by4Canvas `shouldBe` Left "Only horizontal and vertical lines are supported"
        it "Given a DrawRectangle command then the Canvas should contain the rectangle" $ do
            let initialCanvas = read "\
                \----------------------\n\
                \|                    |\n\
                \|xxxxxx              |\n\
                \|     x              |\n\
                \|     x              |\n\
                \----------------------" :: Canvas
            let command = DrawRectangleCommand (Coordinates 16 1) (Coordinates 20 3)
            applyCommand command initialCanvas `shouldBe` Right (read "\
                \----------------------\n\
                \|               xxxxx|\n\
                \|xxxxxx         x   x|\n\
                \|     x         xxxxx|\n\
                \|     x              |\n\
                \----------------------" :: Canvas)
        it "Given a Clear command with a non-empty Canvas then return a blank Canvas of the same dimensions" $ do
            let initialCanvas = read "\
                \----------------------\n\
                \|               xxxxx|\n\
                \|xxxxxx         x   x|\n\
                \|     x         xxxxx|\n\
                \|     x              |\n\
                \----------------------" :: Canvas
            applyCommand ClearCommand initialCanvas `shouldBe` Right blank20by4Canvas
        it "Given a BucketFill command in an open area then the Canvas should fill appropriately" $ do
            let initialCanvas = read "\
                \----------------------\n\
                \|               xxxxx|\n\
                \|xxxxxx         x   x|\n\
                \|     x         xxxxx|\n\
                \|     x              |\n\
                \----------------------" :: Canvas
            let command = BucketFillCommand (Coordinates 10 3) 'o'
            applyCommand command initialCanvas `shouldBe` Right (read "\
                \----------------------\n\
                \|oooooooooooooooxxxxx|\n\
                \|xxxxxxooooooooox   x|\n\
                \|     xoooooooooxxxxx|\n\
                \|     xoooooooooooooo|\n\
                \----------------------" :: Canvas)
        it "Given a BucketFill command in a complicated open area then the Canvas should fill appropriately" $ do
            let initialCanvas = read "\
                \----------------------\n\
                \|               xxxxx|\n\
                \|xxxxxx         x   x|\n\
                \|     x   xxxxx xxxxx|\n\
                \|     x   x   x      |\n\
                \----------------------" :: Canvas
            let command = BucketFillCommand (Coordinates 10 2) 'o'
            applyCommand command initialCanvas `shouldBe` Right (read "\
                \----------------------\n\
                \|oooooooooooooooxxxxx|\n\
                \|xxxxxxooooooooox   x|\n\
                \|     xoooxxxxxoxxxxx|\n\
                \|     xooox   xoooooo|\n\
                \----------------------" :: Canvas)
        it "Given a BucketFill command in a closed area then the Canvas should fill appropriately" $ do
            let initialCanvas = read "\
                \----------------------\n\
                \|               xxxxx|\n\
                \|xxxxxx         x   x|\n\
                \|     x         xxxxx|\n\
                \|     x              |\n\
                \----------------------" :: Canvas
            let command = BucketFillCommand (Coordinates 17 2) 'q'
            applyCommand command initialCanvas `shouldBe` Right (read "\
                \----------------------\n\
                \|               xxxxx|\n\
                \|xxxxxx         xqqqx|\n\
                \|     x         xxxxx|\n\
                \|     x              |\n\
                \----------------------" :: Canvas)
        it "Given a BucketFill command on a non-empty origin position then do nothing" $ do
            let initialCanvas = read "\
                \----------------------\n\
                \|               xxxxx|\n\
                \|xxxxxx         x   x|\n\
                \|     x         xxxxx|\n\
                \|     x              |\n\
                \----------------------" :: Canvas
            let command = BucketFillCommand (Coordinates 4 2) 'q'
            applyCommand command initialCanvas `shouldBe` Right initialCanvas