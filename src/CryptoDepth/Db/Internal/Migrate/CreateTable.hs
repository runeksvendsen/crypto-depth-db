module CryptoDepth.Db.Internal.Migrate.CreateTable
( createInitial
, deleteInitial
, CryptoDepthDb
)
where

import CryptoDepth.Db.Internal.Prelude
import CryptoDepth.Db                               (CryptoDepthDb(..))
import CryptoDepth.Db.Internal.Table.Run
import CryptoDepth.Db.Internal.Table.RunSymbol
import CryptoDepth.Db.Internal.Table.Book
import CryptoDepth.Db.Internal.Table.Path
import OrderBook.Types

import           Database.Beam                      as B
import           Database.Beam.Backend.SQL.SQL92    (IsSql92DataTypeSyntax, doubleType)
import           Database.Beam.Backend.SQL.SQL2003  (IsSql2008BigIntDataTypeSyntax, bigIntType)
import           Database.Beam.Migrate              (DataType(DataType))
import           Database.Beam.Migrate.SQL.Tables
import           Database.Beam.Migrate.Types
import           Database.Beam.Postgres
import           Database.Beam.Postgres.Syntax

import           Data.Text (toLower)


symVenueDT :: DataType PgDataTypeSyntax (NonEmpty SymVenue)
symVenueDT = DataType pgTextType

ordersDT :: DataType PgDataTypeSyntax (Vector SomeOrder)
ordersDT = DataType (pgUnboundedArrayType pgTextType)

quantityDT :: IsSql2008BigIntDataTypeSyntax syntax =>
    DataType syntax (Tagged (OneDiv denominator) (Amount numeraire))
quantityDT = DataType doubleType    -- TODO: Change when resolved: https://github.com/tathougies/beam/issues/324

deleteInitial
    :: CheckedDatabaseSettings Postgres CryptoDepthDb
    -> Migration PgCommandSyntax ()
deleteInitial (CryptoDepthDb a b c d e f g) = do
    dropTable a
    dropTable b
    dropTable c
    dropTable d
    dropTable e
    dropTable f
    dropTable g

createInitial :: () -> Migration PgCommandSyntax (CheckedDatabaseSettings Postgres CryptoDepthDb)
createInitial () =
    CryptoDepthDb
        <$> runTable
        <*> bookTable
        <*> symbolTable
        <*> pathTable
        <*> pathTable
        <*> pathTable
        <*> pathTable

runTable :: Migration PgCommandSyntax (CheckedDatabaseEntity Postgres CryptoDepthDb (TableEntity RunT))
runTable =
    createTable "runs" $ Run
        (field "id" serial unique notNull)
        (field "time" timestamp notNull)

bookTable :: Migration PgCommandSyntax (CheckedDatabaseEntity Postgres CryptoDepthDb (TableEntity BookT))
bookTable =
    createTable "books" $ Book
        (RunId (field "run__id" int))
        (field "venue" text notNull)
        (field "base" text notNull)
        (field "quote" text notNull)
        (field "bids" ordersDT notNull)
        (field "asks" ordersDT notNull)

symbolTable :: Migration PgCommandSyntax (CheckedDatabaseEntity Postgres CryptoDepthDb (TableEntity RunSymbolT))
symbolTable =
    createTable "symbols" $ RunSymbol
        (RunId (field "run__id" int))
        (field "sym" text notNull)

pathTable
  :: forall numeraire.
     KnownSymbol numeraire
  => Migration PgCommandSyntax
      (CheckedDatabaseEntity Postgres CryptoDepthDb (TableEntity (PathT numeraire)))
pathTable =
    createTable (toLower $ toS tableName) $ Path
        (RunId (field "run__id" int))
        (field "src" text notNull)
        (field "dst" text notNull)
        (field "path" symVenueDT notNull)
        (field "qty01" quantityDT notNull)
        (field "qty05" quantityDT notNull)
        (field "qty1" quantityDT notNull)
        (field "qty5" quantityDT notNull)
  where
    tableName = "paths_" <> symbolVal (Proxy :: Proxy numeraire)
