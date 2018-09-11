{-# LANGUAGE TypeApplications #-}
module CryptoDepth.Db.Internal.Migrate.Run
( runMigration )
where

import qualified CryptoDepth.Db.Internal.Migrate.Schema as Schema
import qualified Database.Beam.Postgres         as Postgres
import qualified Data.ByteString.Lazy.Char8     as BSL

import           Control.Monad.IO.Class         (liftIO)

import           Database.Beam.Migrate.Simple   (runSimpleMigration,
                                                 simpleMigration)
import           Database.Beam.Postgres         (Pg, PgCommandSyntax, Postgres)
import           Database.Beam.Postgres.Migrate (migrationBackend)
import           Database.Beam.Postgres.Syntax  (fromPgCommand,
                                                 pgRenderSyntaxScript)

runMigration :: Postgres.Connection -> IO ()
runMigration conn =
  liftIO $ do
    mcommands <- simpleMigration migrationBackend conn Schema.checkedDB
    case mcommands of
      Nothing ->
        fail "Something went wrong constructing migration"
      Just [] ->
        putStrLn "Already up to date"
      Just commands ->
        runSimpleMigration @PgCommandSyntax @Postgres @_ @Pg conn commands
