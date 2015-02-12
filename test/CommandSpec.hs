{-# LANGUAGE OverloadedStrings #-}

module CommandSpec where

import Test.Hspec
import Command

spec :: Spec
spec = do
    describe "parseCommand" $ do
        it "can parse a new canvas command" $ do
            parseCommand "C 20 4" `shouldBe` Just (NewCanvasCommand 20 4)
        it "can parse a draw line command" $ do
            parseCommand "L 2 1 2 4" `shouldBe` Just (DrawLineCommand (Coordinates 2 1) (Coordinates 2 4))
        it "can parse a draw rectangle command" $ do
            parseCommand "R 1 1 3 4" `shouldBe` Just (DrawRectangleCommand (Coordinates 1 1) (Coordinates 3 4))
        it "can parse a bucket fill command" $ do
            parseCommand "B 3 4 o" `shouldBe` Just (BucketFillCommand (Coordinates 3 4) 'o')
        it "can parse a clear command" $ do
            parseCommand "CLR" `shouldBe` Just ClearCommand
