{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text
import Command
import Canvas
import Drawing

main :: IO ()
main = do
    putStrLn "Welcome to Console Draw"
    drawingConsole emptyCanvas

drawingConsole :: Canvas -> IO ()
drawingConsole canvas = do
    enteredCommand <- getLine
    if (((toLower . pack) enteredCommand) == "q")
        then return ()
        else do
            case (parseAndApply enteredCommand canvas) of
                Right newCanvas -> do
                    putStrLn . show $ newCanvas
                    drawingConsole newCanvas
                Left err        -> do
                    putStrLn err
                    drawingConsole canvas

parseAndApply :: String -> Canvas -> Either String Canvas
parseAndApply enteredCommand canvas =
    case (parseCommand enteredCommand) of
        Just command -> applyCommand command canvas
        Nothing      -> Left "Command not recognised"
