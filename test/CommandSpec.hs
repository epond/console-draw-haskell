{-# LANGUAGE OverloadedStrings #-}

module CommandSpec where

import Test.Hspec
import Command

spec :: Spec
spec = do
    describe "parseCommand" $ do
        it "can parse a new canvas" $ do
            parseCommand "C 20 4" `shouldBe` Just (NewCanvasCommand 20 4)
