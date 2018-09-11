module CryptoDepth.Db.Internal.Migrate.Schema
  ( module Current
    , migration
    , db
    , checkedDB ) where

import           Database.Beam               (DatabaseSettings)
import           Database.Beam.Migrate.Types hiding (migrateScript)
import           Database.Beam.Postgres      (PgCommandSyntax, Postgres)


import           CryptoDepth.Db.Internal.Migrate.CreateTable       as Current


migration :: MigrationSteps PgCommandSyntax () (CheckedDatabaseSettings Postgres Current.CryptoDepthDb)
migration = migrationStep "Initial migration" Current.initialMigration

db :: DatabaseSettings Postgres Current.CryptoDepthDb
db = unCheckDatabase checkedDB

checkedDB :: CheckedDatabaseSettings Postgres Current.CryptoDepthDb
checkedDB = evaluateDatabase migration