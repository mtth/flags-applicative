{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

-- | Simple flags parsing module, inspired by @optparse-applicative@.
--
-- Sample usage (note the default log level and optional context):
--
-- @
-- module Main where
--
-- import Control.Applicative ((\<|\>), optional)
-- import Data.Text (Text)
-- import Data.Text.Read (decimal)
-- import Flags.Applicative
-- import System.Environment (getArgs)
--
-- data Options = Options
--   { rootPath :: Text
--   , logLevel :: Int
--   , context :: Maybe Text
--   } deriving Show
--
-- optionsParser :: FlagParser Options
-- optionsParser = Options \<$\> textFlag "root" "path to the root"
--                         \<*\> (numericFlag decimal "log_level" "" \<|\> pure 0)
--                         \<*\> (optional $ textFlag "context" "")
--
-- main :: IO ()
-- main = do
--   args <- getArgs
--   print $ parseFlags optionsParser args
-- @

-- TODO: Add @--help@ support.

module Flags.Applicative
  ( Name, FlagParser, FlagError(..), ParserError(..)
  , parseFlags
  -- * Nullary flags
  , boolFlag
  -- * Unary flags
  , unaryFlag, textFlag, numericFlag
  ) where

import Control.Applicative ((<|>), Alternative, empty, optional)
import Control.Monad (when)
import Control.Monad.Except (Except, catchError, runExcept, throwError)
import Control.Monad.RWS.Strict (RWST, runRWST)
import Control.Monad.Reader (asks)
import Control.Monad.State.Strict (get, modify, put)
import Control.Monad.Writer.Strict (tell)
import Data.Bifunctor (first, second)
import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Map.Strict (Map)
import Data.Maybe (isJust)
import Data.Set (Set)
import Data.Text (Text)
import System.Environment (getArgs)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Read as T

-- | The name of a flag, can use all characters but @=@ (the value delimiter). It's good practice
-- for flag names to be lowercase ASCII with underscores.
type Name = Text

data Arity = Nullary | Unary deriving Eq
data Flag = Flag Arity Text

-- The errors which can happen during flag parsing.
data ValueError
  = MissingValue Name
  | InvalidValue Name Text String

type Action a = RWST
  (Map Name Text) -- Flag values (or empty for nullary flags).
  (Set Name) -- Read flags.
  (Set Name) -- Used flags.
  (Except ValueError) -- Eventual parsing error.
  a

-- | Parser definition errors.
data ParserError
  = DuplicateFlag Name -- ^ The same flag name was declared multiple times.
  | Empty -- ^ The parser is empty (this should not happen if you use standard combinators).
  deriving (Eq, Show)

-- | Flags parser.
--
-- There are two types of flags:
--
-- * Nullary flags created with 'boolFlag' which are 'True' when set and 'False' otherwise (a.k.a.
-- switches). For example @--version@ or @--enable_foo@.
-- * Unary flags created with 'unaryFlag' and its convenience variants (e.g. 'textFlag',
-- 'numericFlag'). These expect a value to be passed in either after an equal sign (@--foo=value@)
-- or as the following input value (@--foo value@). If the value starts with @--@, only the first
-- form is accepted.
--
-- You can run a parser using 'parseFlags'.
data FlagParser a
  = Actionable (Action a) (Map Name Flag)
  | Invalid ParserError
  deriving Functor

-- Returns the combined map of flags if there are no duplicates, otherwise the name of one of the
-- duplicate flags.
mergeFlags :: Map Name Flag -> Map Name Flag -> Either Name (Map Name Flag)
mergeFlags flags1 flags2 = case Map.minViewWithKey $ flags1 `Map.intersection` flags2 of
  Just ((name, _), _) -> Left name
  Nothing -> Right $ flags1 `Map.union` flags2

instance Applicative FlagParser where
  pure res = Actionable (pure res) Map.empty

  Invalid err <*> _ = Invalid err
  _ <*> Invalid err = Invalid err
  Actionable action1 flags1 <*> Actionable action2 flags2 =
    case mergeFlags flags1 flags2 of
      Left name -> Invalid $ DuplicateFlag name
      Right flags -> Actionable (action1 <*> action2) flags

instance Alternative FlagParser where
  empty = Invalid Empty

  Invalid Empty <|> parser = parser
  parser <|> Invalid Empty = parser
  Invalid err <|> _ = Invalid err
  _ <|> Invalid err = Invalid err
  Actionable action1 flags1 <|> Actionable action2 flags2 = case mergeFlags flags1 flags2 of
    Left name -> Invalid $ DuplicateFlag name
    Right flags -> Actionable action flags where
      wrap action = catchError (Just <$> action) $ \case
        (MissingValue _) -> pure Nothing
        err -> throwError err
      action = do
        used <- get
        wrap action1 >>= \case
          Nothing -> put used >> action2
          Just res -> do
            used' <- get
            _ <- wrap action2
            put used'
            pure res

-- | The possible parsing errors.
data FlagError
  -- | At least one unary flag was specified multiple times with different values.
  = InconsistentFlagValues Name
  -- | A unary flag's value failed to parse.
  | InvalidFlagValue Name Text String
  -- | The parser is invalid. Unlike other 'FlagError' constructors, this indicates an issue with
  -- the parser's declaration (rather than the input tokens).
  | InvalidParser ParserError
  -- | A required flag was missing.
  | MissingFlag Name
  -- | A unary flag was missing a value. This can happen either if a value-less unary flag was the
  -- last token or was followed by a value which is also a flag name (in which case you should use
  -- the single-token form: @--flag=--value@).
  | MissingFlagValue Name
  -- | At least one flag was set but unused. This can happen when optional flags are set but their
  -- branch is not selected.
  | UnexpectedFlags (NonEmpty Name)
  -- | An unknown flag was set.
  | UnknownFlag Name
  deriving (Eq, Show)

-- Tries to gather all raw flag values into a map.
gatherValues :: Map Name Flag -> [Text] -> Either FlagError ((Map Name Text), [Text])
gatherValues flags = go where
  go [] = Right (Map.empty, [])
  go (token:tokens) = case T.stripPrefix "--" token of
    Nothing -> second (token:) <$> go tokens
    Just "" -> Right (Map.empty, tokens)
    Just suffix ->
      let
        (name, pval) = T.breakOn "=" suffix
        missing = Left $ MissingFlagValue name
        insert val tokens' = do
          (vals', args') <- go tokens'
          case Map.lookup name vals' of
            Nothing -> Right (Map.insert name val vals', args')
            Just val' -> if val == val'
              then Right (vals', args')
              else Left $ InconsistentFlagValues name
      in case Map.lookup name flags of
        Nothing -> Left (UnknownFlag name)
        Just (Flag Nullary _) -> insert "" tokens
        Just (Flag Unary _) -> case T.uncons pval of
          Nothing -> case tokens of
            (token':tokens') -> if T.isPrefixOf "--" token'
              then missing
              else insert token' tokens'
            _ -> missing
          Just (_, val) -> insert val tokens

-- | Runs a parser on a list of tokens, returning the parsed flags alongside other non-flag
-- arguments (i.e. which don't start with @--@). If the special @--@ token is found, all following
-- tokens will be considered arguments (even if they look like flags).
parseFlags :: FlagParser a -> [String] -> Either FlagError (a, [Text])
parseFlags parser tokens = case parser of
  Invalid err -> Left $ InvalidParser err
  Actionable action flags -> case gatherValues flags (fmap T.pack tokens) of
    Left err -> Left err
    Right (values, args) -> case runExcept $ runRWST action values Set.empty of
      Right (res, usedNames, readNames) ->
        let unused = Set.difference readNames usedNames
        in case Set.minView unused of
          Nothing -> Right (res, args)
          Just (name, names) -> Left $ UnexpectedFlags $ name :| toList names
      Left (MissingValue name) -> Left $ MissingFlag name
      Left (InvalidValue name val msg) -> Left $ InvalidFlagValue name val msg

useFlag :: Name -> Action ()
useFlag name = tell (Set.singleton name) >> modify (Set.insert name)

-- | Returns a nullary parser with the given name and description.
boolFlag :: Name -> Text -> FlagParser Bool
boolFlag name desc = Actionable action flags where
  action = do
    present <- asks (Map.member name)
    when present $ useFlag name
    pure present
  flags = Map.singleton name (Flag Nullary desc)

-- | Returns a unary parser using the given parsing function, name, and description.
unaryFlag :: (Text -> Either String a) -> Name -> Text -> FlagParser a
unaryFlag convert name desc = Actionable action flags where
  action = do
    useFlag name
    asks (Map.lookup name) >>= \case
      Nothing -> throwError $ MissingValue name
      Just val -> case convert val of
        Left err -> throwError $ InvalidValue name val err
        Right res -> pure res
  flags = Map.singleton name (Flag Unary desc)

-- | Returns a flag with 'Text' values.
textFlag :: Name -> Text -> FlagParser Text
textFlag = unaryFlag Right

-- | Returns a flag which can parse numbers using the helper methods in "Data.Text.Read" (e.g.
-- 'Data.Text.Read.decimal').
numericFlag :: T.Reader a -> Name -> Text -> FlagParser a
numericFlag reader = unaryFlag go where
  go val = case reader val of
    Right (res, "") -> Right res
    Left msg -> Left msg
    _ -> Left "trailing value data"
