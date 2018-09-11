{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeApplications #-}
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
import Database.Beam.Postgres.Syntax    (PgValueSyntax)
import           Database.PostgreSQL.Simple.FromField
import           Text.Read
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL

import Data.Typeable
import Data.Scientific  (Scientific)


-- Encode
instance HasSqlValueSyntax be String => HasSqlValueSyntax be (NonEmpty SymVenue) where
  sqlValueSyntax = autoSqlValueSyntax

-- NB: loses precision (Rational -> Double)
instance HasSqlValueSyntax be Double => HasSqlValueSyntax be (Money.Dense currency) where
  sqlValueSyntax = (sqlValueSyntax :: Double -> be) . realToFrac . toRational

instance Json.ToJSON SomeOrder
instance Json.FromJSON SomeOrder

instance HasSqlValueSyntax be BS.ByteString => HasSqlValueSyntax be SomeOrder where
  sqlValueSyntax = (sqlValueSyntax :: BS.ByteString -> be) . toS . Json.encode

instance HasSqlValueSyntax be BS.ByteString => HasSqlValueSyntax be (Vector SomeOrder) where
  sqlValueSyntax = (sqlValueSyntax :: BS.ByteString -> be) . toS . Json.encode


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
  fromField f bsM = do
    double <- fromField f bsM
    let denseM = Money.dense . (toRational :: Double -> Rational) $ double
    maybe (returnError ConversionFailed f "Failed to parse 'Dense' from 'Double'") pure denseM
instance KnownSymbol currency => FromBackendRow Postgres (Money.Dense currency)

instance FromField SomeOrder where
  fromField = parseFromField (Json.decode . toS)
instance FromBackendRow Postgres SomeOrder
