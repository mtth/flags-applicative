{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative ((<|>), optional)
import Data.Text (Text, pack)
import Data.Text.Read (decimal)
import Flags.Applicative
import System.Environment (getArgs)

data Options = Options
  { rootPath :: Text
  , logLevel :: Int
  , context :: Maybe Text
  } deriving Show

optionsParser :: FlagParser Options
optionsParser = Options <$> textFlag "root" "path to the root"
                        <*> (numericFlag decimal "log_level" "" <|> pure 0)
                        <*> (optional $ textFlag "context" "")

main :: IO ()
main = do
  args <- getArgs
  print $ parseFlags optionsParser args
