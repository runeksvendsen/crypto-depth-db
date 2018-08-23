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
