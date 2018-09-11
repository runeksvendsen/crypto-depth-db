module CryptoDepth.Db.Internal.Migrate.CreateTable
( initialMigration
, CryptoDepthDb
)
where

import CryptoDepth.Db.Internal.Prelude
import CryptoDepth.Db                               (CryptoDepthDb(CryptoDepthDb))
import CryptoDepth.Db.Internal.Table.Run
import CryptoDepth.Db.Internal.Table.Book
import CryptoDepth.Db.Internal.Table.Path
import OrderBook.Types

import           Database.Beam                      as B
import           Database.Beam.Backend.SQL.SQL92    (IsSql92DataTypeSyntax, doubleType)
import           Database.Beam.Migrate              (DataType(DataType))
import           Database.Beam.Migrate.SQL.Tables
import           Database.Beam.Migrate.Types
import           Database.Beam.Postgres
import           Database.Beam.Postgres.Syntax


symVenueDT :: DataType PgDataTypeSyntax (NonEmpty SymVenue)
symVenueDT = DataType (pgUnboundedArrayType pgTextType)

ordersDT :: DataType PgDataTypeSyntax (Vector SomeOrder)
ordersDT = DataType (pgUnboundedArrayType pgJsonType)

quantityDT :: IsSql92DataTypeSyntax syntax =>
    DataType syntax (Tagged (OneDiv denominator) (Dense numeraire))
quantityDT = DataType doubleType

initialMigration :: () -> Migration PgCommandSyntax (CheckedDatabaseSettings Postgres CryptoDepthDb)
initialMigration () = CryptoDepthDb
  <$> createTable "run_info"
      ( Run
            (field "id" serial unique notNull)
            (field "time" timestamp notNull)
      )
  <*> createTable "books"
      ( Book
            (RunId (field "run_id" int))
            (field "venue" text notNull)
            (field "base" text notNull)
            (field "quote" text notNull)
            (field "bids" ordersDT notNull)
            (field "asks" ordersDT notNull)
      )
  <*> pathTable "paths_usd"
  <*> pathTable "paths_eur"
  <*> pathTable "paths_gbp"
  <*> pathTable "paths_jpy"

pathTable
  :: KnownSymbol numeraire
  => Text
  -> Migration PgCommandSyntax
      (CheckedDatabaseEntity Postgres CryptoDepthDb (TableEntity (PathT numeraire)))
pathTable tableName =
  createTable tableName
    ( Path
          (RunId (field "run_id" int))
          (field "src" text)
          (field "dst" text)
          (field "path" symVenueDT)
          (field "qty_01" quantityDT notNull)
          (field "qty_05" quantityDT notNull)
          (field "qty_1" quantityDT notNull)
          (field "qty_5" quantityDT notNull)
    )
