module Command where

-- todo use type String -> Maybe CanvasCommand
parseCommand :: String -> Maybe String
parseCommand c = Just $ "Parsed " ++ c