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
-- optionsParser :: FlagsParser Options
-- optionsParser = Options \<$\> textFlag "root" "path to the root"
--                         \<*\> (autoFlag "log_level" "" \<|\> pure 0)
--                         \<*\> (optional $ textFlag "context" "")
--
-- main :: IO ()
-- main = do
--   (opts, args) <- parseSystemFlagsOrDie optionsParser
--   print opts
-- @

module Flags.Applicative (
  -- * Types
  Name, Description, FlagsParser, FlagError(..),
  -- * Running parsers
  parseFlags, parseSystemFlagsOrDie,
  -- * Declaring flags
  -- ** Nullary flags
  switch, boolFlag,
  -- ** Unary flags
  flag, textFlag, hostFlag, autoFlag, textListFlag, autoListFlag
) where

import Control.Applicative ((<|>), Alternative, empty)
import Control.Monad (when)
import Control.Monad.Except (Except, catchError, runExcept, throwError)
import Control.Monad.RWS.Strict (RWST, runRWST)
import Control.Monad.Reader (asks)
import Control.Monad.State.Strict (get, modify, put)
import Data.Bifunctor (second)
import Data.Foldable (foldl', toList)
import Data.List (isPrefixOf)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Map.Strict (Map)
import Data.Set (Set)
import Data.Semigroup ((<>))
import Data.Text (Text)
import Network.Socket (HostName, PortNumber)
import System.Exit (die)
import System.Environment (getArgs)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import Text.Read (readEither)

-- The prefix used to identify all flags.
prefix :: String
prefix = "--"

-- | The name of a flag (without the @--@ prefix). Names can use all valid utf-8 characters except
-- @=@ (the value delimiter). In general, it's good practice for flag names to be lowercase ASCII
-- with underscores.
--
-- The following names are reserved and attempting to define a flag with the same name will cause an
-- error:
--
-- * @help@, displays usage when set.
-- * @swallowed_flags@, flags in this list which are set but undeclared will be ignored rather than
-- cause an error during parsing.
-- * @swallowed_switches@, similar to @swallowed_flags@ but for switches (nullary flags).
type Name = Text

-- Add the flag prefix to a name.
qualify :: Name -> Text
qualify name = T.pack prefix <> name

-- | An human-readable explanation of what the flag does. It is displayed when the parser is invoked
-- with the @--help@ flag.
type Description = Text

data Arity = Nullary | Unary deriving Eq

data Flag = Flag Arity Description

-- The errors which can happen during flag parsing.
data ValueError
  = InvalidValue Name Text String
  | MissingValues (NonEmpty Name)

missingValue :: Name -> ValueError
missingValue name = MissingValues $ name :| []

type Action a = RWST
  (Map Name Text) -- Flag values (or empty for nullary flags).
  () -- Unused.
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
  go (Exactly name) = case Map.lookup name flags of
    Just (Flag Unary _) -> qualify name <> "=*"
    _ -> qualify name
  go (AllOf s) =
    T.intercalate " " $ fmap go $ filter (/= emptyUsage) $ toList s
  go (OneOf s) =
    let contents s' = T.intercalate "|" $ fmap go $ toList $ s'
    in if Set.member emptyUsage s
      then "[" <> contents (Set.delete emptyUsage s) <> "]"
      else "(" <> contents s <> ")"
  describe (name, Flag _ desc) = if T.null desc then "" else "\n" <> qualify name <> "\t" <> desc
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
-- * Nullary flags created with 'switch' and 'boolFlag', which do not accept a value.
-- * Unary flags created with 'flag' and its convenience variants (e.g. 'textFlag', 'autoFlag',
-- 'autoListFlag'). These expect a value to be passed in either after an equal sign (@--foo=value@)
-- or as the following input value (@--foo value@). If the value starts with @--@, only the first
-- form is accepted.
--
-- You can run a parser using 'parseFlags' or 'parseSystemFlagsOrDie'.
data FlagsParser a
  = Actionable (Action a) (Map Name Flag) Usage
  | Invalid ParserError
  deriving Functor

-- Returns the combined map of flags if there are no duplicates, otherwise the name of one of the
-- duplicate flags.
mergeFlags :: Map Name Flag -> Map Name Flag -> Either Name (Map Name Flag)
mergeFlags flags1 flags2 = case Map.minViewWithKey $ flags1 `Map.intersection` flags2 of
  Just ((name, _), _) -> Left name
  Nothing -> Right $ flags1 `Map.union` flags2

instance Applicative FlagsParser where
  pure res = Actionable (pure res) Map.empty emptyUsage

  Invalid err <*> _ = Invalid err
  _ <*> Invalid err = Invalid err
  Actionable action1 flags1 usage1 <*> Actionable action2 flags2 usage2 =
    case mergeFlags flags1 flags2 of
      Left name -> Invalid $ Duplicate name
      Right flags -> Actionable (action1 <*> action2) flags (usage1 `andAlso` usage2)

instance Alternative FlagsParser where
  empty = Invalid Empty

  Invalid Empty <|> parser = parser
  parser <|> Invalid Empty = parser
  Invalid err <|> _ = Invalid err
  _ <|> Invalid err = Invalid err
  Actionable action1 flags1 usage1 <|> Actionable action2 flags2 usage2 =
    case mergeFlags flags1 flags2 of
      Left name -> Invalid $ Duplicate name
      Right flags -> Actionable action flags (usage1 `orElse` usage2) where
        wrap a = catchError (Right <$> a) $ \case
          (MissingValues names) -> pure $ Left names
          err -> throwError err
        action = do
          used <- get
          wrap action1 >>= \case
            Left names -> do
              put used
              wrap action2 >>= \case
                Left names' -> throwError $ MissingValues $ names <> names'
                Right res -> pure res
            Right res -> do
              used' <- get
              _ <- wrap action2
              put used'
              pure res

-- | The possible parsing errors.
data FlagError
  -- | A flag was declared multiple times.
  = DuplicateFlag Name
  -- | The parser was empty.
  | EmptyParser
  -- | The input included the @--help@ flag.
  | Help Text
  -- | At least one unary flag was specified multiple times with different values.
  | InconsistentFlagValues Name
  -- | A unary flag's value failed to parse.
  | InvalidFlagValue Name Text String
  -- | A required flag was missing; at least one of the returned flags should be set.
  | MissingFlags (NonEmpty Name)
  -- | A unary flag was missing a value. This can happen either if a value-less unary flag was the
  -- last token or was followed by a value which is also a flag name (in which case you should use
  -- the single-token form: @--flag=--value@).
  | MissingFlagValue Name
  -- | A flag with a reserved name was declared.
  | ReservedFlag Name
  -- | A nullary flag was given a value.
  | UnexpectedFlagValue Name
  -- | At least one flag was set but unused. This can happen when optional flags are set but their
  -- branch is not selected.
  | UnexpectedFlags (NonEmpty Name)
  -- | An unknown flag was set.
  | UnknownFlag Name
  deriving (Eq, Show)

displayFlags :: Foldable f => f Name -> Text
displayFlags = T.intercalate " " . fmap qualify . toList

-- Pretty-print a 'FlagError'.
displayFlagError :: FlagError -> Text
displayFlagError (DuplicateFlag name) = qualify name <> " was declared multiple times"
displayFlagError EmptyParser = "empty parser"
displayFlagError (Help usage) = usage
displayFlagError (InconsistentFlagValues name) = "inconsistent values for " <> qualify name
displayFlagError (InvalidFlagValue name val msg) =
  "invalid value \"" <> val <> "\" for " <> qualify name <> " (" <> T.pack msg <> ")"
displayFlagError (MissingFlags names) =
  "at least one of the following required flags must be set: " <> displayFlags names
displayFlagError (MissingFlagValue name) = "missing value for " <> qualify name
displayFlagError (ReservedFlag name) = qualify name <> " was declared but is reserved"
displayFlagError (UnexpectedFlagValue name) = "unexpected value for " <> qualify name
displayFlagError (UnexpectedFlags names) = "unexpected " <> displayFlags names
displayFlagError (UnknownFlag name) = "undeclared " <> qualify name

-- Mark a flag as used. This is useful to check for unexpected flags after parsing is complete.
useFlag :: Name -> Action ()
useFlag name = modify (Set.insert name)

-- | Returns a parser with the given name and description for a flag with no value, failing if the
-- flag is not present. See also 'boolFlag' for a variant which doesn't fail when the flag is
-- missing.
switch :: Name -> Description -> FlagsParser ()
switch name desc = Actionable action flags usage where
  action = asks (Map.member name) >>= \case
    True -> useFlag name
    False -> throwError $ missingValue name
  flags = Map.singleton name (Flag Nullary desc)
  usage = Exactly name

-- | Returns a parser with the given name and description for a flag with no value, returning
-- whether the flag was present.
boolFlag :: Name -> Description -> FlagsParser Bool
boolFlag name desc = (True <$ switch name desc) <|> pure False

-- | Returns a parser using the given parsing function, name, and description for a flag with an
-- associated value.
flag :: (Text -> Either String a) -> Name -> Description -> FlagsParser a
flag convert name desc = Actionable action flags usage where
  action = do
    useFlag name
    asks (Map.lookup name) >>= \case
      Nothing -> throwError $ missingValue name
      Just val -> case convert val of
        Left err -> throwError $ InvalidValue name val err
        Right res -> pure res
  flags = Map.singleton name (Flag Unary desc)
  usage = Exactly name

-- | Returns a parser for a single text value.
textFlag :: Name -> Description -> FlagsParser Text
textFlag = flag Right

-- | Returns a parser for network hosts of the form @hostname:port@. The port part is optional.
hostFlag :: Name -> Description -> FlagsParser (HostName, Maybe PortNumber)
hostFlag = flag $ \txt -> do
  let (hostname, suffix) = T.breakOn ":" txt
  mbPort <- case T.stripPrefix ":" suffix of
      Nothing -> Right Nothing
      Just portStr -> Just <$> readEither (T.unpack portStr)
  pure (T.unpack hostname, mbPort)

-- | Returns a parser for any value with a 'Read' instance. Prefer 'textFlag' for textual values
-- since 'flag'  will expect its values to be double-quoted and might not work as expected.
autoFlag :: Read a => Name -> Description -> FlagsParser a
autoFlag = flag (readEither . T.unpack)

-- | Returns a parser for a single flag with multiple text values.
textListFlag :: Text -> Name -> Description -> FlagsParser [Text]
textListFlag sep =  flag $ Right . T.splitOn sep

-- | Returns a parser for a single flag with multiple values having a 'Read' instance, with a
-- configurable separator. Empty values are always ignored, so it's possible to declare an empty
-- list as @--list=@ and trailing commas are supported.
autoListFlag :: Read a => Text -> Name -> Description -> FlagsParser [a]
autoListFlag sep =
  flag $ sequenceA . fmap (readEither . T.unpack) . filter (not . T.null) . T.splitOn sep

-- Tries to gather all raw flag values into a map. When @ignoreUnknown@ is true, this function will
-- pass through any unknown flags into the returned argument list, otherwise it will throw a
-- 'FlagError'.
gatherValues :: Bool -> Map Name Flag -> [String] -> Either FlagError ((Map Name Text), [String])
gatherValues ignoreUnknown flags = go where
  go [] = Right (Map.empty, [])
  go (token:tokens) = if not (prefix `isPrefixOf` token)
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
            Nothing -> if ignoreUnknown
              then second (token:) <$> go tokens
              else Left (UnknownFlag name)
            Just (Flag Nullary _) -> if T.null pval
              then insert "" tokens
              else Left $ UnexpectedFlagValue name
            Just (Flag Unary _) -> case T.uncons pval of
              Nothing -> case tokens of
                (token':tokens') -> if prefix `isPrefixOf` token'
                  then missing
                  else insert (T.pack token') tokens'
                _ -> missing
              Just (_, val) -> insert val tokens

-- Runs a single parsing pass.
runAction :: Bool -> Action a -> Map Name Flag -> [String] -> Either FlagError (a, Set Name, [String])
runAction ignoreUnknown action flags tokens = case gatherValues ignoreUnknown flags tokens of
  Left err -> Left err
  Right (values, args) -> case runExcept $ runRWST action values Set.empty of
    Right (rv, usedNames, _) ->
      let unused = Set.difference (Map.keysSet values) usedNames
      in Right (rv, unused, args)
    Left (MissingValues names) -> Left $ MissingFlags names
    Left (InvalidValue name val msg) -> Left $ InvalidFlagValue name val msg

-- Preprocessing parser.
reservedParser :: FlagsParser (Bool, Set Name, Set Name)
reservedParser =
  let textSetFlag name = Set.fromList <$> (flag (Right . T.splitOn ",") name "" <|> pure [])
  in (,,)
    <$> boolFlag "help" ""
    <*> textSetFlag "swallowed_flags"
    <*> textSetFlag "swallowed_switches"

-- | Runs a parser on a list of tokens, returning the parsed flags alongside other non-flag
-- arguments (i.e. which don't start with @--@). If the special @--@ token is found, all following
-- tokens will be considered arguments even if they look like flags.
parseFlags :: FlagsParser a -> [String] -> Either FlagError (a, [String])
parseFlags parser tokens = case reservedParser of
  Invalid _ -> error "unreachable"
  Actionable action0 flags0 _ -> do
    ((showHelp, sflags, sswitches), _, tokens') <- runAction True action0 flags0 tokens
    (action, flags) <- case parser of
      Invalid (Duplicate name) -> Left $ DuplicateFlag name
      Invalid Empty -> Left EmptyParser
      Actionable action flags usage -> do
        case Set.lookupMin (Map.keysSet $ Map.intersection flags0 flags) of
          Nothing -> Right ()
          Just name -> Left $ ReservedFlag name
        when showHelp $  Left (Help $ displayUsage flags usage)
        let
          flags' = foldl' (\m name -> Map.insert name (Flag Unary "") m) flags sflags
          flags'' = foldl' (\m name -> Map.insert name (Flag Nullary "") m) flags' sswitches
        Right (action, flags'')
    (rv, unused, tokens'') <- runAction False action flags tokens'
    case Set.minView $ Set.difference unused (sflags <> sswitches) of
      Nothing -> Right (rv, tokens'')
      Just (name, names) -> Left $ UnexpectedFlags $ name :| toList names

-- | Runs a parser on the system's arguments, or exits with code 1 and prints the relevant error
-- message in case of failure.
parseSystemFlagsOrDie :: FlagsParser a -> IO (a, [String])
parseSystemFlagsOrDie parser = parseFlags parser <$> getArgs >>= \case
  Left err -> die $ T.unpack $ displayFlagError err
  Right res -> pure res
