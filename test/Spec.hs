{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

import Control.Applicative ((<|>))
import Data.Either (isLeft)
import Data.List.NonEmpty (NonEmpty(..))
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
      res `shouldBe` Left (DuplicateFlag "foo")
    it "should support help" $ do
      let
        parser = textFlag "foo" ""
        res = parseFlags parser ["--foo=abc", "hi", "--help"]
      isLeft res `shouldBe` True
    it "should fail on unknown flags" $ do
      let
        parser = textFlag "foo" ""
        res = parseFlags parser ["hi", "--bar"]
      res `shouldBe` Left (UnknownFlag "bar")
    it "should detect unexpected flags" $ do
      let
        parser = switch "bar" "" <|> switch "foo" ""
        res = parseFlags parser ["--bar", "--foo"]
      res `shouldBe` Left (UnexpectedFlags ("foo" :| []))
    it "should branch correctly with unary flags" $ do
      let
        parser = (Right <$> autoFlag @String "ok" "") <|> (Left <$> autoFlag @String "fail" "")
        res = parseFlags parser ["--ok", "\"yes\"", "no"]
      res `shouldBe` Right (Right "yes", ["no"])
    it "should branch correctly with nullary flags" $ do
      let
        parser = (True <$ switch "true" "") <|> (False <$ switch "false" "")
        res = parseFlags parser ["--true", "b", "a"]
      res `shouldBe` Right (True, ["b", "a"])
    it "should fail on inconsistent flag values" $ do
      let
        parser = textFlag "foo" ""
        res = parseFlags parser ["--foo=1", "--foo=2"]
      res `shouldBe` Left (InconsistentFlagValues "foo")
    it "should support the same flag value multiple times" $ do
      let
        parser = autoFlag @Int "foo" ""
        res = parseFlags parser ["--foo=1", "--foo=1"]
      res `shouldBe` Right (1, [])
    it "should support text lists" $ do
      let
        parser = textListFlag "," "bar" ""
        res = parseFlags parser ["--bar=a,b,c", "def"]
      res `shouldBe` Right (["a", "b", "c"], ["def"])
    it "should swallow switches" $ do
      let
        parser = boolFlag "foo" ""
        res = parseFlags parser ["--foo", "--bar", "--swallowed_switches=bar"]
      res `shouldBe` Right (True, [])
    it "should fail when a switch is set as a flag" $ do
      let
        parser = boolFlag "foo" ""
        res = parseFlags parser ["--foo=3"]
      res `shouldBe` Left (UnexpectedFlagValue "foo")
    it "should swallow flags" $ do
      let
        parser = boolFlag "foo" ""
        res = parseFlags parser ["--bar=2", "--swallowed_flags=bar"]
      res `shouldBe` Right (False, [])
    it "should fail when a flag is swallowed as a switch" $ do
      let
        parser = boolFlag "foo" ""
        res = parseFlags parser ["--foo", "--bar=1", "--swallowed_switches=bar"]
      res `shouldBe` Left (UnexpectedFlagValue "bar")
    it "should parse a hostname" $ do
      let
        parser = hostFlag "host" ""
        res = parseFlags parser ["--host=foo.com"]
      res `shouldBe` Right (("foo.com", Nothing), [])
    it "should parse a hostname and a port" $ do
      let
        parser = hostFlag "host" ""
        res = parseFlags parser ["--host=localhost:1234"]
      res `shouldBe` Right (("localhost", Just 1234), [])
    it "should fail when given an invalid port" $ do
      let
        parser = hostFlag "host" ""
        res = parseFlags parser ["--host=localhost:1a2"]
      case res of
        Left (InvalidFlagValue "host" _ _) -> pure ()
        _ -> expectationFailure $ show res
    it "should report all missing required flags" $ do
      let
        parser = switch "foo" "" <|> switch "bar" ""
        res = parseFlags parser []
      case res of
        Left (MissingFlags ("foo" :| ["bar"])) -> pure ()
        _ -> expectationFailure $ show res
