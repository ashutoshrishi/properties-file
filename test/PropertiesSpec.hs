{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module PropertiesSpec where

import           Data.Text                                ( Text )
import           Data.Aeson
import           Data.Aeson.Encoding
import qualified Data.ByteString.Lazy.Char8    as BL
import           Data.Properties
import           Data.Word                                ( Word32
                                                          , Word64
                                                          )
import           GHC.Generics
import           Test.Tasty
import           Test.Tasty.Golden
import           Test.Tasty.HUnit

test_kplProps :: TestTree
test_kplProps =
  goldenVsString "KPL properties" "test/golden/kpl.golden"
    $   BL.pack
    .   show
    <$> decodeFile "test/golden/kpl.properties"

test_kplJSON :: TestTree
test_kplJSON =
  testCase "KPL properties via JSON" $ do
    parsed <- decodeFileJSON @KPLConfig "test/golden/kpl.properties"
    expected <- eitherDecode @KPLConfig <$> BL.readFile "test/golden/kpl.json"
    parsed @?= expected

data KPLConfig = KPLConfig { _AggregationEnabled    :: Bool
                           , _AggregationMaxCount   :: Word64
                           , _AggregationMaxSize    :: Word64
                           , _CollectionMaxCount    :: Word64
                           , _CollectionMaxSize     :: Word64
                           , _ConnectTimeout        :: Word64
                           , _KinesisEndpoint       :: Text
                           , _LogLevel              :: Text
                           , _MaxConnections        :: Word64
                           , _MetricsGranularity    :: Text
                           , _MetricsLevel          :: Text
                           , _MetricsNamespace      :: Text
                           , _MetricsUploadDelay    :: Word64
                           , _MinConnections        :: Word64
                           , _RateLimit             :: Word64
                           , _RecordMaxBufferedTime :: Word64
                           , _RecordTtl             :: Word64
                           , _Region                :: Text
                           , _RequestTimeout        :: Word64
                           , _VerifyCertificate     :: Bool
                           , _ThreadingModel        :: ThreadConfig
                           , _ThreadPoolSize        :: Word32
                           } deriving (Generic, Eq, Show)

data ThreadConfig = ThreadPerRequest | ThreadPooled deriving (Show, Eq, Generic)

instance FromJSON ThreadConfig where
  parseJSON = withText "ThreadConfig" match
   where
    match "PER_REQUEST" = pure ThreadPerRequest
    match "POOLED"      = pure ThreadPooled
    match _             = fail "Bad ThreadConfig value"

instance ToJSON ThreadConfig where
  toEncoding ThreadPerRequest = text "PER_REQUEST"
  toEncoding ThreadPooled     = text "POOLED"

instance FromJSON KPLConfig where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = drop 1 }

instance ToJSON KPLConfig where
  toEncoding = genericToEncoding defaultOptions { fieldLabelModifier = drop 1 }



