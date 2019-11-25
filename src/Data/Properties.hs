module Data.Properties
  ( parseFile
  , parseProps
  , parseAsJSON
  , fetch
  , Properties
  , Value(..)
  , Key
  )
where

import qualified Data.Aeson                    as A
import qualified Data.Attoparsec.Text          as P
import qualified Data.HashMap.Strict           as Map
import           Data.Properties.Parser
import           Data.Text                                ( Text )
import qualified Data.Text.IO                  as TIO

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
parseAsJSON :: A.FromJSON a => Properties -> Either String a
parseAsJSON = resultToEither . A.fromJSON . toJSONObject

resultToEither :: A.Result a -> Either String a
resultToEither (A.Error msg) = Left msg
resultToEither (A.Success v) = Right v

toJSONObject :: Properties -> A.Value
toJSONObject = A.Object . Map.map toJSONValue

toJSONValue :: Value -> A.Value
toJSONValue (String v) = A.String v
toJSONValue (Number v) = A.Number v
toJSONValue (Bool   v) = A.Bool v
