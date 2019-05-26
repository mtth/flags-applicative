{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative ((<|>), optional)
import Data.Text (Text, pack)
import Data.Text.Read (decimal, double)
import Flags.Applicative
import System.Environment (getArgs)

data Options = Options
  { rootPath :: Text
  , logLevel :: Int
  , context :: Maybe Text
  , values :: [Double]
  } deriving Show

optionsParser :: FlagParser Options
optionsParser = Options <$> (textFlag "root" "path to the root" <|> textFlag "url" "")
                        <*> (autoFlag "log_level" "" <|> pure 0)
                        <*> (optional $ textFlag "context" "")
                        <*> (autoListFlag "," "values" "" <|> pure [])

main :: IO ()
main = parseSystemFlagsOrDie optionsParser >>= print
