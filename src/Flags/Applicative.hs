{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Simple flags parsing module, inspired by @optparse-applicative@.
--
-- Sample usage (note the default log level and optional context):
--
-- @
-- module Main where
--
-- import Control.Applicative ((\<|\>), optional)
-- import Data.Text (Text)
-- import Flags.Applicative
--
-- data Options = Options
--   { rootPath :: Text
--   , logLevel :: Int
--   , context :: Maybe Text
--   } deriving Show
--
-- optionsParser :: FlagParser Options
-- optionsParser = Options \<$\> textFlag "root" "path to the root"
--                         \<*\> (flag "log_level" "" \<|\> pure 0)
--                         \<*\> (optional $ textFlag "context" "")
--
-- main :: IO ()
-- main = do
--   (opts, args) <- parseSystemFlagsOrDie optionsParser
--   print opts
-- @

-- TODO: Add @--help@ support.

module Flags.Applicative
  ( Name, Description, FlagParser, FlagError(..)
  , parseFlags, parseSystemFlagsOrDie
  -- * Defining flags
  , switch, unaryFlag, textFlag, flag, repeatedTextFlag, repeatedFlag
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
import Data.List (isPrefixOf)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Map.Strict (Map)
import Data.Maybe (isJust)
import Data.Set (Set)
import Data.Text (Text)
import System.Exit (die)
import System.Environment (getArgs)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import Text.Read (readEither)

-- | The name of a flag, can use all valid utf-8 characters but @=@ (the value delimiter). In
-- general, it's good practice for flag names to be lowercase ASCII with underscores.
--
-- The following names are reserved and attempting to define a flag with the same name will cause an
-- error:
--
-- * @help@, used to display usage when set.
type Name = Text

-- The name of the help switch.
helpName :: Name
helpName = "help"

-- | An human-readable explanation of what the flag does.
type Description = Text

data Arity = Nullary | Unary deriving Eq
data Flag = Flag Arity Description

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

data Usage
  = Exactly Name
  | AllOf (Set Usage)
  | OneOf (Set Usage)
  deriving (Eq, Ord)

emptyUsage :: Usage
emptyUsage = AllOf Set.empty

andAlso :: Usage -> Usage -> Usage
andAlso (AllOf s1) (AllOf s2) = AllOf $ s1 <> s2
andAlso (AllOf s) u = AllOf $ Set.insert u s
andAlso u (AllOf s) = AllOf $ Set.insert u s
andAlso u1 u2 = AllOf $ Set.fromList [u1, u2]

orElse :: Usage -> Usage -> Usage
orElse (OneOf s1) (OneOf s2) = OneOf $ s1 <> s2
orElse (OneOf s) u = OneOf $ Set.insert u s
orElse u (OneOf s) = OneOf $ Set.insert u s
orElse u1 u2 = OneOf $ Set.fromList [u1, u2]

displayUsage :: Map Name Flag -> Usage -> Text
displayUsage flags usage = "usage: " <> go usage <> "\n" <> details where
  go (Exactly name) =
    let prefix = "--" <> name
    in case Map.lookup name flags of
      Just (Flag Unary _) -> prefix <> "=*"
      _ -> prefix
  go (AllOf s) =
    T.intercalate " " $ fmap go $ filter (/= emptyUsage) $ toList s
  go (OneOf s) =
    let contents s' = T.intercalate "|" $ fmap go $ toList $ s'
    in if Set.member emptyUsage s
      then "[" <> contents (Set.delete emptyUsage s) <> "]"
      else "(" <> contents s <> ")"
  describe (name, Flag _ desc) =
    let prefix = "--" <> name
    in if T.null desc then "" else "\n" <> prefix <> "\t" <> desc
  details = T.concat $ fmap describe $ Map.toList flags

-- Parser definition errors.
data ParserError
  = Duplicate Name
  | Empty
  deriving (Eq, Show)

-- | Flags parser.
--
-- There are two types of flags:
--
-- * Nullary flags created with 'switch which are 'True' when set and 'False' otherwise. For example
-- @--version@ or @--enable_foo@.
-- * Unary flags created with 'unaryFlag' and its convenience variants (e.g. 'textFlag', 'flag',
-- 'repeatedFlag'). These expect a value to be passed in either after an equal sign (@--foo=value@)
-- or as the following input value (@--foo value@). If the value starts with @--@, only the first
-- form is accepted.
--
-- You can run a parser using 'parseFlags'.
data FlagParser a
  = Actionable (Action a) (Map Name Flag) Usage
  | Invalid ParserError
  deriving Functor

-- Returns the combined map of flags if there are no duplicates, otherwise the name of one of the
-- duplicate flags.
mergeFlags :: Map Name Flag -> Map Name Flag -> Either Name (Map Name Flag)
mergeFlags flags1 flags2 = case Map.minViewWithKey $ flags1 `Map.intersection` flags2 of
  Just ((name, _), _) -> Left name
  Nothing -> Right $ flags1 `Map.union` flags2

instance Applicative FlagParser where
  pure res = Actionable (pure res) Map.empty emptyUsage

  Invalid err <*> _ = Invalid err
  _ <*> Invalid err = Invalid err
  Actionable action1 flags1 usage1 <*> Actionable action2 flags2 usage2 =
    case mergeFlags flags1 flags2 of
      Left name -> Invalid $ Duplicate name
      Right flags -> Actionable (action1 <*> action2) flags (usage1 `andAlso` usage2)

instance Alternative FlagParser where
  empty = Invalid Empty

  Invalid Empty <|> parser = parser
  parser <|> Invalid Empty = parser
  Invalid err <|> _ = Invalid err
  _ <|> Invalid err = Invalid err
  Actionable action1 flags1 usage1 <|> Actionable action2 flags2 usage2 =
    case mergeFlags flags1 flags2 of
      Left name -> Invalid $ Duplicate name
      Right flags -> Actionable action flags (usage1 `orElse` usage2) where
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
  -- | A flag was declared multiple times.
  = DuplicateFlag Name
  -- | The input included the @--help@ flag.
  | Help Text
  -- | At least one unary flag was specified multiple times with different values.
  | InconsistentFlagValues Name
  -- | A unary flag's value failed to parse.
  | InvalidFlagValue Name Text String
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

-- Pretty-print a 'FlagError'.
displayFlagError :: FlagError -> Text
displayFlagError (InconsistentFlagValues name) = "inconsistent values for --" <> name
displayFlagError (InvalidFlagValue name val msg) =
  "invalid value \"" <> val <> "\" for --" <> name <> " (" <> T.pack msg <> ")"
displayFlagError (DuplicateFlag name) = "--" <> name <> " was declared multiple times"
displayFlagError (Help usage) = usage
displayFlagError (MissingFlag name) = "--" <> name <> " is required but was not set"
displayFlagError (MissingFlagValue name) = "missing value for --" <> name
displayFlagError (UnexpectedFlags names) =
  "unexpected " <> (T.intercalate " " $ fmap ("--" <>) $ toList $ names)
displayFlagError (UnknownFlag name) = "undeclared --" <> name

-- Mark a flag as used. This is useful to check for unexpected flags after parsing is complete.
useFlag :: Name -> Action ()
useFlag name = tell (Set.singleton name) >> modify (Set.insert name)

-- | Returns a nullary parser with the given name and description.
switch :: Name -> Description -> FlagParser Bool
switch name desc = Actionable action flags usage where
  action = do
    present <- asks (Map.member name)
    when present $ useFlag name
    pure present
  flags = Map.singleton name (Flag Nullary desc)
  usage = emptyUsage `orElse` Exactly name

-- | Returns a unary parser using the given parsing function, name, and description.
unaryFlag :: (Text -> Either String a) -> Name -> Description -> FlagParser a
unaryFlag convert name desc = Actionable action flags usage where
  action = do
    useFlag name
    asks (Map.lookup name) >>= \case
      Nothing -> throwError $ MissingValue name
      Just val -> case convert val of
        Left err -> throwError $ InvalidValue name val err
        Right res -> pure res
  flags = Map.singleton name (Flag Unary desc)
  usage = Exactly name

-- | Returns a parser for a single text value.
textFlag :: Name -> Description -> FlagParser Text
textFlag = unaryFlag Right

-- | Returns a parser for any value with a 'Read' instance. Prefer 'textFlag' for textual values
-- since 'flag'  will expect its values to be double-quoted and might not work as expected.
flag :: Read a => Name -> Description -> FlagParser a
flag = unaryFlag (readEither . T.unpack)

-- | Returns a parser for a multiple text value.
repeatedTextFlag :: Text -> Name -> Description -> FlagParser [Text]
repeatedTextFlag sep =  unaryFlag $ Right . T.splitOn sep

-- | Returns a parser for multiple values with a 'Read' instance, with a configurable separator.
repeatedFlag :: Read a => Text -> Name -> Description -> FlagParser [a]
repeatedFlag sep = unaryFlag $ sequenceA . fmap (readEither . T.unpack) . T.splitOn sep

helpSwitch :: FlagParser Bool
helpSwitch = switch helpName "show usage and exit"

-- Tries to gather all raw flag values into a map.
gatherValues :: Map Name Flag -> [String] -> Either FlagError ((Map Name Text), [String])
gatherValues flags = go where
  go [] = Right (Map.empty, [])
  go (token:tokens) = if not (isPrefixOf "--" token)
    then second (token:) <$> go tokens
    else
      let entry = drop 2 token :: String
      in if entry == ""
        then Right (Map.empty, tokens)
        else
          let
            (name, pval) = T.breakOn "=" (T.pack entry)
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
                (token':tokens') -> if isPrefixOf "--" token'
                  then missing
                  else insert (T.pack token') tokens'
                _ -> missing
              Just (_, val) -> insert val tokens

-- | Runs a parser on a list of tokens, returning the parsed flags alongside other non-flag
-- arguments (i.e. which don't start with @--@). If the special @--@ token is found, all following
-- tokens will be considered arguments (even if they look like flags).
parseFlags :: FlagParser a -> [String] -> Either FlagError (a, [String])
parseFlags parser tokens = case (,) <$> helpSwitch <*> parser of
  Invalid (Duplicate name) -> Left $ DuplicateFlag name
  Actionable action flags usage -> case gatherValues flags tokens of
    Left err -> Left err
    Right (values, args) -> case runExcept $ runRWST action values Set.empty of
      Right ((True, _), _, _) -> Left $ Help $ displayUsage flags usage
      Right ((False, res), usedNames, readNames) ->
        let unused = Set.difference readNames usedNames
        in case Set.minView unused of
          Nothing -> Right (res, args)
          Just (name, names) -> Left $ UnexpectedFlags $ name :| toList names
      Left (MissingValue name) -> Left $ MissingFlag name
      Left (InvalidValue name val msg) -> Left $ InvalidFlagValue name val msg
  _ -> error "unreachable" -- The parser can never be empty.

-- | Runs a parser on the system's arguments, or exits with code 1 and prints the relevant error
-- message in case of failure.
parseSystemFlagsOrDie :: FlagParser a -> IO (a, [String])
parseSystemFlagsOrDie parser = parseFlags parser <$> getArgs >>= \case
  Left err -> die $ T.unpack $ displayFlagError err
  Right res -> pure res
