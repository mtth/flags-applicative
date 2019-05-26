{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative ((<|>), optional)
import Data.Text (Text, pack)
import Flags.Applicative
import System.Environment (getArgs)

data Mode = Lenient | Strict deriving (Read, Show)

data Options = Options
  { rootPath :: Text
  , logLevel :: Int
  , mode :: Mode
  , context :: Maybe (Text, [Double])
  } deriving Show

optionsParser :: FlagParser Options
optionsParser = Options
  <$> (textFlag "root" "path to the root" <|> ("HERE" <$ switch "default_root" ""))
  <*> autoFlag "log_level" "log verbosity"
  <*> (autoFlag "mode" "mode [Lenient|Strict]" <|> pure Strict)
  <*> (optional $ (,) <$> textFlag "context" "" <*> (autoListFlag "," "values" "" <|> pure []))

main :: IO ()
main = parseSystemFlagsOrDie optionsParser >>= print
