module CryptoDepth.Db.Internal.Migrate.Run
( createTables
, dropTables
, assertTables
)
where

import           CryptoDepth.Db.Internal.Prelude
import qualified CryptoDepth.Db.Internal.Migrate.Schema as Schema
import qualified Data.ByteString.Lazy.Char8             as BSL
import           Control.Monad                          (void, when)
import           Control.Monad.IO.Class                 (liftIO)
import           Control.Exception                      (catch)

import           Database.Beam.Migrate.Simple
import           Database.Beam.Migrate.Types            (collectChecks)
import qualified Database.Beam.Postgres                 as Pg
import qualified Database.Beam.Postgres.Migrate         as Pg
import qualified Database.Beam.Postgres.Syntax          as Pg
import qualified Database.PostgreSQL.Simple             as PgSimple
import qualified Database.PostgreSQL.Simple.Types       as PgSimple


createTables :: Pg.Connection -> IO (CheckedDatabaseSettings Pg.Postgres Schema.CryptoDepthDb)
createTables conn = runMigration conn Schema.dbCreate

-- | Assert that the database has been migrated to the current schema
assertTables :: Pg.Connection -> IO ()
assertTables conn = do
    actualDbState   <- Pg.getDbConstraints conn
    expectedDbState <- collectChecks <$> mkCheckedDb Schema.dbCreate
    when (actualDbState /= expectedDbState) $
        error $ unlines
            [ "Unsupported DB schema found:"
            , show actualDbState
            ]
  where
    mkCheckedDb migration =
        runMigrationSteps 0 Nothing migration (\_ _ step -> pure (runMigrationSilenced step))

dropTables
  :: Pg.Connection
  -> CheckedDatabaseSettings Pg.Postgres Schema.CryptoDepthDb
  -> IO ()
dropTables conn checkedDb = runMigration conn (Schema.dbDelete checkedDb)

runMigration conn migration =
    let executeFunction = (tryExecute conn <=< debugPrintQuery) . newSqlQuery
        tryExecute conn query =
            catch (void $ PgSimple.execute_ conn query)
            (\err -> putStrLn ("ERROR: " ++ show (err :: PgSimple.SqlError)))
    in runMigrationSteps 0 Nothing migration
          (\_ _ -> executeMigration executeFunction)

debugPrintQuery :: PgSimple.Query -> IO PgSimple.Query
debugPrintQuery query =
    putStrLnErr (toS $ PgSimple.fromQuery query) >> return query

newSqlQuery :: Pg.PgCommandSyntax -> PgSimple.Query
newSqlQuery syntax =
    PgSimple.Query (toS sqlFragment)
  where
    sqlFragment = Pg.pgRenderSyntaxScript . Pg.fromPgCommand $ syntax
