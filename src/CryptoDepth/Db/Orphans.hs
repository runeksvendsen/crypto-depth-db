{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module CryptoDepth.Db.Orphans where

import CryptoDepth.Db.Internal.Prelude

import Database.Beam.Backend.SQL.SQL92  (HasSqlValueSyntax(..), autoSqlValueSyntax)
import CryptoDepth.Types                (SymVenue, Amount)
import OrderBook.Types                  (SomeOrder)
import Data.List.NonEmpty               (NonEmpty)
import qualified Money
import qualified Data.Aeson             as Json
import Database.Beam.Backend
import Database.Beam.Postgres
import Database.Beam.Postgres.Syntax    (PgValueSyntax)
import           Database.PostgreSQL.Simple.FromField
import           Database.PostgreSQL.Simple.ToField
import           Text.Read
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL

import Data.Typeable
import Debug.Trace

-- Encode
instance HasSqlValueSyntax be String => HasSqlValueSyntax be (NonEmpty SymVenue) where
  sqlValueSyntax = autoSqlValueSyntax

-- TODO: Change when resolved: https://github.com/tathougies/beam/issues/324
instance HasSqlValueSyntax be Double => HasSqlValueSyntax be (Amount currency) where
  sqlValueSyntax = (sqlValueSyntax :: Double -> be) . fromIntegral

instance Json.ToJSON SomeOrder
instance Json.FromJSON SomeOrder

instance HasSqlValueSyntax be BS.ByteString => HasSqlValueSyntax be SomeOrder where
  sqlValueSyntax = (sqlValueSyntax :: BS.ByteString -> be) . toS . Json.encode

instance ToField SomeOrder where
  toField = toField @BS.ByteString . toS . Json.encode

-- instance HasSqlValueSyntax be BS.ByteString => HasSqlValueSyntax be (Vector SomeOrder) where
--   sqlValueSyntax = (sqlValueSyntax :: BS.ByteString -> be) . toS . Json.encode


-- Decode
parseFromField
  :: forall a. Typeable a
  => (BS.ByteString -> Maybe a)
  -> Field
  -> Maybe BS.ByteString
  -> Conversion a
parseFromField parseFn f bs =
    let err = printf "Could not parse value for '%s'" (show $ typeOf (undefined :: a)) in
    parseFn <$> fromField f bs
      >>= maybe (returnError ConversionFailed f err) pure

instance FromField (NonEmpty SymVenue) where
  fromField = parseFromField (readMaybe . toS)
instance FromBackendRow Postgres (NonEmpty SymVenue)

instance KnownSymbol currency => FromField (Amount currency) where
  fromField f bsM =
    -- TODO: Change when resolved: https://github.com/tathougies/beam/issues/324
    fromIntegral . round @Double <$> fromField f bsM
instance KnownSymbol currency => FromBackendRow Postgres (Amount currency)

instance FromField SomeOrder where
  fromField = parseFromField (Json.decode . toS)
instance FromBackendRow Postgres SomeOrder
