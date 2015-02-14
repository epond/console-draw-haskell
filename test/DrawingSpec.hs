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
