{-# LANGUAGE OverloadedStrings #-}

module CanvasSpec where

import Test.Hspec
import Canvas

canvasEmpty = "\
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
            show (createNewCanvas 20 4) `shouldBe` canvasEmpty
        it "can determine width" $ do
            canvasWidth (createNewCanvas 10 5) `shouldBe` 10
            canvasWidth emptyCanvas `shouldBe` 0
        it "can determine height" $ do
            canvasHeight (createNewCanvas 10 5) `shouldBe` 5
            canvasHeight emptyCanvas `shouldBe` 0

main = hspec spec