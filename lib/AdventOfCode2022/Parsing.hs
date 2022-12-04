{-# LANGUAGE ViewPatterns #-}

module AdventOfCode2022.Parsing where

import Data.Attoparsec.Text
import qualified Data.Text as T

unsafeParse :: Show a => Parser a -> String -> a
unsafeParse p s = handle $ parse p (T.pack s)
  where
    handle r = case r of
      Done (T.unpack -> "") a -> a
      Done a b -> error $ "done with remaining" <> show a <> " " <> show b
      Fail a b c -> error $ "failed with" <> show a <> " " <> show b <> " " <> show c
      Partial f -> handle $ f (T.pack "")
