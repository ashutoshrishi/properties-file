{-# LANGUAGE OverloadedStrings #-}
module ParserSpec where

import           Data.Text                                ( Text )
import           Data.Attoparsec.Text              hiding ( parse )
import           Data.Properties.Parser
import           Test.Tasty.HUnit

unit_boolParsing :: Assertion
unit_boolParsing = do
  parse value "True" @?= Right (Bool True)
  parse value "true" @?= Right (Bool True)
  parse value "False" @?= Right (Bool False)
  parse value "false" @?= Right (Bool False)

unit_numberParsing :: Assertion   
unit_numberParsing = do
  parse value "123" @?= Right (Number 123)
  parse value "10.23" @?= Right (Number 10.23)
  parse value "-3.2" @?= Right (Number (-3.2))
  parse value "000" @?= Right (Number 0)
  parse value "12foo" @?= Left "endOfInput"

unit_stringParsing :: Assertion
unit_stringParsing = do
  parse value "hello" @?= Right (String "hello")
  parse value "foo bar" @?= Right (String "foo bar")
    -- parse value "a\nb" `shouldBe` Right (String "a\nb")

-- parseOnly which does not discard input
parse :: Parser a -> Text -> Either String a
parse parser = parseOnly (parser <* endOfInput)





