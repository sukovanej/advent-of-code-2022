module CliArguments (parseArgs, CliArguments(..)) where

import Text.Read

data CliArguments = CliArguments { day :: Maybe Int, inputFileName :: Maybe String }

dayArguments :: Int -> CliArguments
dayArguments x = CliArguments (Just x) Nothing

inputFileNameArguments :: String -> CliArguments
inputFileNameArguments x = CliArguments Nothing (Just x)

instance Semigroup CliArguments where
  a <> b = CliArguments (maybe (day a) Just (day b)) (maybe (inputFileName a) Just (inputFileName b))

instance Monoid CliArguments where
  mempty = CliArguments { day = Nothing, inputFileName = Nothing }

parseArgs :: [String] -> Either String CliArguments
parseArgs ("-f":filename:rest) = (inputFileNameArguments filename <>) <$> parseArgs rest
parseArgs ("-d":dayStr:rest) = do 
  day <- maybe (Left $ "Expected number, got" <> dayStr) Right $ parseNumber dayStr
  (dayArguments day <>) <$> parseArgs rest
parseArgs ([]) = Right mempty
parseArgs _ = Left $ "Failed to parse input arguments"

parseNumber :: String -> Maybe Int
parseNumber = readMaybe
