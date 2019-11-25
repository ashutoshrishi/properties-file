{-# LANGUAGE OverloadedStrings #-}
module Data.Properties where

import           Control.Applicative                      ( (<|>) )
import           Control.Monad                            ( void )
import           Data.Attoparsec.Text                     ( Parser
                                                          , isEndOfLine
                                                          , (<?>)
                                                          )
import qualified Data.Attoparsec.Text          as P
import           Data.Char
import           Data.Functor                             ( ($>) )
import qualified Data.HashMap.Strict           as Map
import           Data.Maybe                               ( catMaybes )
import           Data.Scientific                          ( Scientific )
import           Data.Text                                ( Text )
import qualified Data.Text.IO                  as TIO

import qualified Data.Aeson                    as A

-- | Collection of properties.
type Properties = Map.HashMap Key Value

-- | A single property
type Property = (Key, Value)
type Key = Text

-- | A Property value as a Haskell value.
data Value
  = String !Text
  | Number !Scientific
  | Bool !Bool
  deriving (Show, Eq)

-- | Lookup a property value by the key name.
fetch :: Properties -> Key -> Maybe Value
fetch = flip Map.lookup

-- | Parse properties from a file.
parseFile :: FilePath -> IO (Either String Properties)
parseFile f = parseProps <$> TIO.readFile f

-- | Parse properties from given contents.
parseProps :: Text -> Either String Properties
parseProps = P.parseOnly properties

------------------------------------------------------------------------------
-- JSON interchange                                                         --
------------------------------------------------------------------------------

-- | Parse a Haskell value which already has a 'A.FromJSON' instance, but use
-- the given 'Properties'.
-- 
-- The intermediate JSON object is an 'aeson' value and where the toplevel
-- object as no nested objects, and has only strings, numbers, and boolean
-- fields.
parseAsJSON :: A.FromJSON a => Properties -> A.Result a
parseAsJSON = A.fromJSON . toJSONObject

toJSONObject :: Properties -> A.Value
toJSONObject = A.Object . Map.map toJSONValue

toJSONValue :: Value -> A.Value
toJSONValue (String v) = A.String v
toJSONValue (Number v) = A.Number v
toJSONValue (Bool   v) = A.Bool v

------------------------------------------------------------------------------
-- Parser                                                                   --
------------------------------------------------------------------------------

properties :: Parser Properties
properties = Map.fromList . catMaybes <$> P.many' line <?> "Properties"

-- Parse a single line in a properties file.
-- A line in such a file is either an empty line, comment, or a property.
line :: Parser (Maybe Property)
line = either (const Nothing) Just <$> do
  P.skipWhile isEndOfLine
    *> P.eitherP comment property
    <* P.skipWhile isEndOfLine

comment :: Parser ()
comment =
  do
      void $ P.char '#' <|> P.char '!'
      void $ P.takeWhile (not . isEndOfLine)
    <?> "Comment"

property :: Parser Property
property =
  do
      k <- key
      void $ P.skipSpace *> delimiter <* P.skipSpace
      v <- value
      return (k, v)
    <?> "Property"

value :: Parser Value
value = P.choice
  [ Number <$> P.scientific
  , P.string "True" $> Bool True
  , P.string "true" $> Bool True
  , P.string "False" $> Bool False
  , P.string "false" $> Bool False
  , String <$> P.takeTill isEndOfLine
  ]

key :: Parser Key
key = P.takeWhile1 isIdent <?> "Identifier" where isIdent = isAlphaNum

delimiter :: Parser Char
delimiter = P.skipSpace *> P.satisfy isDelim <* P.skipSpace
 where
  isDelim ':' = True
  isDelim '=' = True
  isDelim _   = False
