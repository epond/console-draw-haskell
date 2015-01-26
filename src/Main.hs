{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Environment
import Data.Text
import Command

main = do
    enteredCommand <- getLine
    if (((toLower . pack) enteredCommand) == "q")
        then return ()
        else do
            case (parseCommand enteredCommand) of
                Just command -> putStrLn command
                Nothing      -> putStrLn "Command not recognised"
            main
