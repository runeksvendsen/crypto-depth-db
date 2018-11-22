{-# LANGUAGE TemplateHaskell #-}
module CryptoDepth.Db.Test.Prepare
( runWithDb
, openConn
)
where

import           CryptoDepth.Db.Internal.Prelude
import           CryptoDepth.Db.Internal.Orphans            ()
import qualified CryptoDepth.Db.Internal.Migrate.Run        as Run
import qualified CryptoDepth.Db.Insert                      as Insert
import qualified CryptoDepth                                as CD

import qualified Data.HashMap.Strict                        as Map
import qualified Database.Beam                              as Beam
import qualified Database.Beam.Postgres                     as Postgres
import qualified Data.Aeson                                 as Json
import qualified Data.ByteString.Lazy                       as BL
import qualified Data.ByteString                            as BS
import qualified Codec.Archive.Zip                          as Zip

import           Control.Exception                          (bracket)
import           Data.Time.Clock                            (getCurrentTime)
import           Data.FileEmbed                             (embedFile, makeRelativeToProject)


-- |
runWithDb
    :: -- | Action to perform each time after storing a list of 'CD.ABook'
       (FilePath -> [CD.ABook] -> Postgres.Connection -> IO ())
       -- | Action to perform after all 'CD.ABook's have been stored
    -> ([(FilePath, [CD.ABook])] -> Postgres.Connection -> IO a)
    -> IO a
runWithDb afterSingleStore afterAllStore = do
    let booksList = either error id $ fileBooksE
    withPreparedDb $ \conn -> do
        forM_ booksList $ \(file, books) -> do
            _ <- mainStore conn books
            afterSingleStore file books conn
        afterAllStore booksList conn
  where
    fileBooksE :: Either String [(FilePath, [CD.ABook])]
    fileBooksE = fileDataE >>= traverse jsonDecode
    fileDataE :: Either String [(FilePath, BL.ByteString)]
    fileDataE = fileAndData <$> decodeZip zipData
    mkErrorStr fileName str = fileName ++ ": " ++ show str
    jsonDecode :: Json.FromJSON a
               => (FilePath, BL.ByteString)
               -> Either String (FilePath, a)
    jsonDecode tuple@(fileName, _) =
        fmapL (mkErrorStr fileName) $ traverse Json.eitherDecode tuple

fileAndData :: Zip.Archive -> [(FilePath, BL.ByteString)]
fileAndData =
    map entryFileData . Zip.zEntries
  where
    entryFileData entry =
        (Zip.eRelativePath entry, Zip.fromEntry entry)

decodeZip :: BS.ByteString -> Either String Zip.Archive
decodeZip =
    Zip.toArchiveOrFail . toS

zipData :: BS.ByteString
zipData =
    $(makeRelativeToProject "test/data/json.zip" >>= embedFile)

withPreparedDb :: (Postgres.Connection -> IO a) -> IO a
withPreparedDb f =
    bracket setup teardown (doStuff f)
  where
    -- Initialize: Open connection + create tables
    setup = do
        conn <- openConn
        checkedDb <- Run.createTables conn
        return (conn, checkedDb)
    -- Clean up: Drop tables + close connection
    teardown (conn, checkedDb) = do
        Run.dropTables conn checkedDb
        Postgres.close conn
    doStuff f (conn, _) = f conn

-- |
openConn :: IO Postgres.Connection
openConn = do
    dbUrl <- dbUrlFromConf
    Postgres.connectPostgreSQL (toS dbUrl)
  where
    dbUrlFromConf = do
        Json.Object dbConf <- either error return =<<
            Json.eitherDecodeFileStrict "test/config/docker.json"
        let dbUrlKey = "db_url"
            (Json.String dbUrl) = fromMaybe (error $ show dbUrlKey ++ " not found") $
                Map.lookup dbUrlKey dbConf
        return dbUrl

mainStore
    :: Postgres.Connection
    -> [CD.ABook]
    -> IO [CD.Sym]
mainStore conn books = do
    time <- getCurrentTime
    Insert.runPGTransactionT
        (Insert.insertAll (Beam.withDatabase conn) time books)
        conn
