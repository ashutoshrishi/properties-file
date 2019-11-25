{-# LANGUAGE OverloadedStrings #-}
module Data.Properties.Parser where


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
