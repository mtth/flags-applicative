# Applicative flags

Simple flags parsing, inspired by
[`optparse-applicative`](http://hackage.haskell.org/package/optparse-applicative):

```haskell
data Options = Options
  { rootPath :: Text
  , logLevel :: Int
  , context :: Maybe Text
  } deriving Show

optionsParser :: FlagParser Options
optionsParser = Options <$> textFlag "root" "path to the root"
                        <*> (numericFlag decimal "log_level" "" <|> pure 0)
                        <*> (optional $ textFlag "context" "")
```

See the [documentation](http://hackage.haskell.org/package/flags-applicative)
for more information.
