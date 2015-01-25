{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Environment
import Data.Text
import CommandInterpreter

main = do
    enteredCommand <- getLine
    if (((toLower . pack) enteredCommand) == "q")
        then return ()
        else do
            putStrLn $ parseCommand enteredCommand
            main
