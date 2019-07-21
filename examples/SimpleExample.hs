#!/usr/bin/env stack
-- stack --install-ghc runghc --package=flags-applicative

{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative ((<|>), optional)
import Data.Text (Text, pack)
import Flags.Applicative

data Mode = Lenient | Strict deriving (Bounded, Enum, Read, Show)

data Flags = Flags
  { rootPath :: Text
  , logLevel :: Int
  , mode :: Mode
  , context :: Maybe (Text, [Double])
  } deriving Show

flagsParser :: FlagsParser Flags
flagsParser = Flags
  <$> (flag textVal "root" "path to the root" <|> ("HERE" <$ switch "default_root" ""))
  <*> flag intVal "log_level" "log verbosity"
  <*> (flag enumVal "mode" "mode [Lenient|Strict]" <|> pure Strict)
  <*> (optional $ (,)
    <$> flag textVal "context" ""
    <*> (flag (listOf fracVal) "values" "" <|> pure []))

main :: IO ()
main = parseSystemFlagsOrDie flagsParser >>= print
