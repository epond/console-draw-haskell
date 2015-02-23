{-# LANGUAGE OverloadedStrings #-}

module CanvasSpec where

import Test.Hspec
import Canvas
import qualified Command as CO

blank20by4Canvas = "\
\----------------------\n\
\|                    |\n\
\|                    |\n\
\|                    |\n\
\|                    |\n\
\----------------------"

canvasWithTwoLines = "\
\----------------------\n\
\|                    |\n\
\|xxxxxx              |\n\
\|     x              |\n\
\|     x              |\n\
\----------------------"

spec :: Spec
spec = do
    describe "Canvas" $ do
        it "can be created from width and height" $ do
            show (createNewCanvas 20 4) `shouldBe` blank20by4Canvas
        it "can read and show as the inverse of each other" $ do
            show (read canvasWithTwoLines :: Canvas) `shouldBe` canvasWithTwoLines
        it "can determine width" $ do
            canvasWidth (createNewCanvas 10 5) `shouldBe` 10
            canvasWidth emptyCanvas `shouldBe` 0
        it "can determine height" $ do
            canvasHeight (createNewCanvas 10 5) `shouldBe` 5
            canvasHeight emptyCanvas `shouldBe` 0
        it "when getNode is called on an empty space then return the empty character" $ do
            getNode (read canvasWithTwoLines :: Canvas) (CO.Coordinates 1 3) `shouldBe` Just emptyPosition
        it "when getNode is called on a line then return the line character" $ do
            getNode (read canvasWithTwoLines :: Canvas) (CO.Coordinates 6 3) `shouldBe` Just 'x'
        it "when getNode is called out of bounds then return Nothing" $ do
            getNode (read canvasWithTwoLines :: Canvas) (CO.Coordinates 1 5) `shouldBe` Nothing
        it "when plot is called with a valid point return the canvas with the point" $ do
            let point = CO.Coordinates 3 2
            let expectedCanvas = read "\
\----------------------\n\
\|                    |\n\
\|  x                 |\n\
\|                    |\n\
\|                    |\n\
\----------------------" :: Canvas
            plot 'x' (read blank20by4Canvas :: Canvas) point `shouldBe` expectedCanvas
        it "when plot is called with an out of bounds point return the original canvas" $ do
            let point = CO.Coordinates 30 2
            plot 'x' (read canvasWithTwoLines :: Canvas) point `shouldBe` (read canvasWithTwoLines :: Canvas)


main = hspec spec