# properties-file

Read java [.properties](https://en.wikipedia.org/wiki/.properties) files into Haskell values.

The current version of the library looks at a the .properties file being a simple flat config file of key-value parameters, where a key is a string, and a value is either a boolean, [`scientific`](https://hackage.haskell.org/package/scientific) number, or a string.

A properties file can be therefore be read into an un-ordered `HashMap` of `Text` keys and `AST` values. The value type is a newtype wrapper on the `Bool`, `Scientific` and `Text` types.

Alternatively, Haskell records can be constructed directly if they have an [aeson](https://hackage.haskell.org/package/aeson) `FromJSON` instance. The intermediate JSON `Value` should be considered to be a flat `Object` with only `Bool`, `Number`, and `String` values. The AST used in this package is a limited subset of the full variance of the `aeson` `Value`.

## Usage

### Hashmap usage

Given a `sample.properties` file with this data:

    hello: world
    volume: 8.123
    acceptable: true

we can read the file into a Haskell `HashMap` type:

    import qualified Data.Properties as Properties

    >>> Right props <- Properties.decodeFile "sample.properties"
    >>> props
    fromList [("hello", String "world"), ("volume", Number 8.123), ("acceptable", Boolean True)])

and read individual values using the field name:

    >>> Properties.fetch props "hello"
    Just (String "world")


### Using `FromJSON` instances

Aeson provides a lot of facilities to generate/write auto-deriving code for your Haskell types. Using its `FromJSON` instances you can parse a `Data.Aeson.Value` (their AST) into a Haskell type. The `FromJSON` instance can be hand written, derived via generics, or generated using template Haskell.

A simple flat .properties config file looked like a flat YAML/JSON file with a different key-value delimiter. Therefore, I decided to leverage the existing deriving mechanisms in the popular `aeson` library and let users think of the parsed properties map as a single level JSON Object, with only String, Number, and Boolean fields.

For the sample example file above, if you have a Haskell record:

    {-# LANGUAGE DeriveGeneric #-}

    import Data.Aeson
    import GHC.Generics

    data Sample =
      Sample { hello      :: Text
             , volume     :: Float
             , acceptable :: Bool
             } deriving (Generic, Show)

    -- The field names match the property keys
    instance FromJSON Sample where
      parseJSON = genericParseJSON defaultOptions


and then while reading the `sample.properties` file, a `Sample` value can be parsed:

    >>> :set -XTypeApplications
    >>> Properties.decodeFileJSON @Sample "sample.properties"
    Right (Sample { hello = "world", volume = 8.123, acceptable = true })

You can use any complicated `parseJSON` implementation as long as you consider the aeson `Value` to be an Object with only `Data.Aeson.String`, `Data.Aeson.Number`,  and `Data.Aeson.Bool` constructors.

maybe you want a 3-tuple instead:

    newtype Sample' = Sample' (String, Double, Bool)

    instance FromJSON Sample' where
        parseJSON = withObject "Sample" $ \v ->
            Sample' <$> v .: "hello"
                    <*> v .: "volume"
                    <*> v .: "acceptable"

The `Properties.decodeFileJSON` will work for such a type just fine.
