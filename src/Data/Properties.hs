module Data.Properties
  (
    -- * Decoding Properties 
    decodeFile
  , decodeProps
    -- * Decoding haskell values via 'FromJSON'
  , decodeFileJSON
    -- * Accessing Properties
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

-- | Parse properties from a @.properties@ file.
decodeFile :: FilePath -> IO (Either String Properties)
decodeFile f = decodeProps <$> TIO.readFile f

-- | Parse properties directly from the given contents.
decodeProps :: Text -> Either String Properties
decodeProps = P.parseOnly properties

-- | Parse a Haskell value which already has a 'A.FromJSON' instance from a
-- @.properties@ file.
--
-- The intermediate JSON object is an aeson 'A.Value' and where the toplevel
-- object has no nested objects, and only strings, numbers, and boolean
-- fields.
decodeFileJSON :: A.FromJSON a => FilePath -> IO (Either String a)
decodeFileJSON f = do
  res <- decodeFile f
  return $ res >>= resultToEither . A.fromJSON . A.toJSON

resultToEither :: A.Result a -> Either String a
resultToEither (A.Error   msg) = Left msg
resultToEither (A.Success v  ) = Right v
