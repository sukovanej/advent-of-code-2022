module CliArguments (parseArgs, CliArguments(..)) where

import Text.Read

data CliArguments = CliArguments { day :: Maybe Int, inputFileName :: Maybe String, solution :: Maybe Int }

dayArguments :: Int -> CliArguments
dayArguments x = CliArguments (Just x) Nothing Nothing

inputFileNameArguments :: String -> CliArguments
inputFileNameArguments x = CliArguments Nothing (Just x) Nothing

solutionArguments :: Int -> CliArguments
solutionArguments x = CliArguments Nothing Nothing (Just x)

instance Semigroup CliArguments where
  a <> b = CliArguments (maybe (day a) Just (day b))
                        (maybe (inputFileName a) Just (inputFileName b))
                        (maybe (solution a) Just (solution b))

instance Monoid CliArguments where
  mempty = CliArguments { day = Nothing, inputFileName = Nothing, solution = Nothing }

parseArgs :: [String] -> Either String CliArguments
parseArgs ("-f":filename:rest) = (inputFileNameArguments filename <>) <$> parseArgs rest
parseArgs ("-d":dayStr:rest) = do 
  parsedArguments <- dayArguments <$> parseNumber dayStr
  nextArguments <- parseArgs rest
  return $ parsedArguments <> nextArguments
parseArgs ("-s":dayStr:rest) = do 
  parsedArguments <- solutionArguments <$> parseNumber dayStr
  nextArguments <- parseArgs rest
  return $ parsedArguments <> nextArguments
parseArgs ([]) = Right mempty
parseArgs _ = Left $ "Failed to parse input arguments"

parseNumber :: String -> Either String Int
parseNumber input = maybe (Left $ "Expected number, got" <> input) Right $ readMaybe input
