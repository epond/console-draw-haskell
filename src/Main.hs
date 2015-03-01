{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text
import Command
import Canvas
import Drawing

main :: IO ()
main = do
    drawingConsole emptyCanvas

-- TODO can this recursion be rewritten to use foldl to ensure tail recursion?
drawingConsole :: Canvas -> IO ()
drawingConsole canvas = do
    -- TODO find a way to avoid this additional newline as putStr doesn't immediately write to stdout 
    putStrLn "enter command: "
    enteredCommand <- getLine
    if "q" == (toLower . pack $ enteredCommand)
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
