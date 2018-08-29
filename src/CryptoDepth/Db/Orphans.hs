{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module CryptoDepth.Db.Orphans where

import CryptoDepth.Db.Internal.Prelude

import Database.Beam.Backend.SQL.SQL92  (HasSqlValueSyntax(..), autoSqlValueSyntax)
import CryptoDepth.Internal.Types       (SymVenue)
import OrderBook.Types                  (SomeOrder)
import Data.List.NonEmpty               (NonEmpty)
import qualified Money
import qualified Data.Aeson             as Json
import Database.Beam.Backend
import Database.Beam.Postgres
import           Database.PostgreSQL.Simple.FromField
import           Text.Read
import qualified Data.ByteString as BS
import Data.Typeable


-- Encode

instance HasSqlValueSyntax be String => HasSqlValueSyntax be (NonEmpty SymVenue) where
  sqlValueSyntax = autoSqlValueSyntax

instance (HasSqlValueSyntax be String, KnownSymbol currency)
    => HasSqlValueSyntax be (Money.Dense currency) where
  sqlValueSyntax = autoSqlValueSyntax

instance Json.ToJSON SomeOrder
instance Json.FromJSON SomeOrder

instance HasSqlValueSyntax be String => HasSqlValueSyntax be SomeOrder where
  sqlValueSyntax = autoSqlValueSyntax

instance HasSqlValueSyntax be String => HasSqlValueSyntax be (Vector SomeOrder) where
  sqlValueSyntax = autoSqlValueSyntax


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

instance KnownSymbol currency => FromField (Money.Dense currency) where
  fromField = parseFromField (readMaybe . toS)
instance KnownSymbol currency => FromBackendRow Postgres (Money.Dense currency)

instance FromField SomeOrder where
  fromField = parseFromField (Json.decode . toS)
instance FromBackendRow Postgres SomeOrder
