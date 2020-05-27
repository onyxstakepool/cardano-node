{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Api.MetaData
  ( readJSONMetaData
  , renderMetaDataError
  ) where

import           Cardano.Prelude hiding (MetaData)
import           Prelude (String)

import           Control.Exception (IOException)
import qualified Control.Exception as Exception
import           Control.Monad.Fail (fail)
import           Data.Aeson
import qualified Data.ByteString.Char8 as SC8
import qualified Data.ByteString.Lazy.Char8 as LC8
import qualified Data.HashMap.Strict as HMS
import           Data.Scientific (floatingOrInteger)
import           Data.Text.Encoding.Base64 (isBase64)
import qualified Data.Text as Text
import qualified Data.Vector as Vector

import           Cardano.Api.Error (textShow)
import           Shelley.Spec.Ledger.MetaData (MetaData(..), MetaDatum(..))


data MetaDataError
  = ConversionErrNullNotAllowed
  | ConversionErrNumberNotInteger
  | ConversionErrLongerThan64Bytes
  | DecodeErrJSON !FilePath !String
  | ReadFileErr !FilePath !String
  deriving (Eq, Ord, Show)


renderMetaDataError :: MetaDataError -> Text
renderMetaDataError err =
  case err of
    ConversionErrNullNotAllowed -> "JSON Null value is not allowed in MetaData"
    ConversionErrNumberNotInteger -> "Only integers are allowed in MetaData"
    ConversionErrLongerThan64Bytes -> "JSON string is longer than 64 bytes"
    DecodeErrJSON fp decErr ->
      "Error decoding JSON at: " <> textShow fp <> " Error: " <> textShow decErr
    ReadFileErr fp rErr ->
      "Error reading file at: " <> textShow fp <> " Error: " <> textShow rErr

-- No 'ToJSON' instance is written for 'MetaData'.
-- It does not make sense to JSON roundtrip test 'MetaData'
-- as 'String's and 'ByteString's are indistinguishable.
instance FromJSON MetaData where
  parseJSON = withObject "MetaData"  $ \v -> MetaData <$> (v .: "mdMap")

instance FromJSON MetaDatum where
  parseJSON v =
    case valueToMetaDatum v of
      Right mDatum -> return mDatum
      Left decErr -> fail . show $ renderMetaDataError decErr

readJSONMetaData :: FilePath -> IO (Either MetaDataError MetaData)
readJSONMetaData fp = do
  eBs <- Exception.try $ LC8.readFile fp
  case eBs of
    Left ioEx -> return . Left . ReadFileErr fp $ handler ioEx
    Right bs -> return . first (DecodeErrJSON fp) $ eitherDecode' bs
 where
  handler :: IOException -> String
  handler e = "Cardano.Api.MetaData.readJSONMetaData: " <> displayException e


valueToMetaDatum :: Value -> Either MetaDataError MetaDatum
valueToMetaDatum Null = Left ConversionErrNullNotAllowed
valueToMetaDatum (Bool bl)
  | bl = Right $ I 1
  | otherwise = Right $ I 0
valueToMetaDatum (Number sci) =
  case (floatingOrInteger sci :: Either Double Integer) of
    Left _ -> Left ConversionErrNumberNotInteger
    Right int -> Right $ I int
valueToMetaDatum (String txt) = byteStringOrText txt
valueToMetaDatum (Array vector) =
  case Vector.mapM valueToMetaDatum vector of
    Left err -> Left err
    Right vecDatums -> Right . List $ Vector.toList vecDatums
valueToMetaDatum (Object hm) =
  case mapM tupleToMetaDatum $ HMS.toList hm of
    Left err -> Left err
    Right tuples -> Right $ Map tuples

-- Helpers

tupleToMetaDatum :: (Text, Value) -> Either MetaDataError (MetaDatum, MetaDatum)
tupleToMetaDatum (k, v) = do
  metaKey <- valueToMetaDatum $ String k
  metaValue <- valueToMetaDatum v
  Right (metaKey, metaValue)

-- | If text is encoded in base64, we convert it to a 'ByteString'.
byteStringOrText :: Text -> Either MetaDataError MetaDatum
byteStringOrText txt
  | Text.length txt > 64 = Left ConversionErrLongerThan64Bytes
  | isBase64 txt = let bs = SC8.pack $ Text.unpack txt
                   in Right $ B bs
  | otherwise = Right $ S txt
