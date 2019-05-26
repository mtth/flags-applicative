{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative ((<|>))
import Data.List.NonEmpty (NonEmpty(..))
import Data.Text.Read (decimal)
import Flags.Applicative
import Test.Hspec
import Test.Hspec.QuickCheck

main :: IO ()
main = hspec $ do
  describe "parse" $ do
    it "should parse a single flag" $ do
      let
        parser = textFlag "foo" ""
        res = parseFlags parser ["--foo=abc", "hi"]
      res `shouldBe` Right ("abc", ["hi"])
    it "should fail on duplicate flag" $ do
      let
        parser = (,) <$> textFlag "foo" "" <*> textFlag "foo" ""
        res = parseFlags parser []
      res `shouldBe` Left (InvalidParser $ DuplicateFlag "foo")
    it "should fail on unknown flags" $ do
      let
        parser = textFlag "foo" ""
        res = parseFlags parser ["hi", "--bar"]
      res `shouldBe` Left (UnknownFlag "bar")
    it "should detect unexpected flags" $ do
      let
        parser = boolFlag "bar" "" <|> boolFlag "foo" ""
        res = parseFlags parser ["--bar", "--foo"]
      res `shouldBe` Left (UnexpectedFlags ("foo" :| []))
    it "should branch correctly with unary flags" $ do
      let
        parser = (Right <$> textFlag "ok" "") <|> (Left <$> textFlag "fail" "")
        res = parseFlags parser ["--ok", "yes", "no"]
      res `shouldBe` Right (Right "yes", ["no"])
    it "should branch correctly with nullary flags" $ do
      let
        parser = (True <$ boolFlag "true" "") <|> (False <$ boolFlag "false" "")
        res = parseFlags parser ["--true", "b", "a"]
      res `shouldBe` Right (True, ["b", "a"])
    it "should fail on inconsistent flag values" $ do
      let
        parser = textFlag "foo" ""
        res = parseFlags parser ["--foo=1", "--foo=2"]
      res `shouldBe` Left (InconsistentFlagValues "foo")
    it "should support the same flag value multiple times" $ do
      let
        parser = numericFlag decimal "foo" "" :: FlagParser Int
        res = parseFlags parser ["--foo=1", "--foo=1"]
      res `shouldBe` Right (1, [])
