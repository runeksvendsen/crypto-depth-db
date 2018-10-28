module CryptoDepth.Db.Internal.Migrate.Schema
( dbCreate
, dbDelete
, Current.CryptoDepthDb
)

where

import           Database.Beam               (DatabaseSettings)
import           Database.Beam.Migrate.Types hiding (migrateScript)
import           Database.Beam.Postgres      (PgCommandSyntax, Postgres)

import           CryptoDepth.Db.Internal.Migrate.CreateTable       as Current


dbCreate :: MigrationSteps PgCommandSyntax () (CheckedDatabaseSettings Postgres Current.CryptoDepthDb)
dbCreate =
    migrationStep "Initial migration" Current.createInitial

dbDelete
    :: CheckedDatabaseSettings Postgres CryptoDepthDb
    -> MigrationSteps PgCommandSyntax () ()
dbDelete checkedDb =
    migrationStep "Delete initial" (\_ -> Current.deleteInitial checkedDb)
