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
import qualified Data.Text                                  as T
import qualified Data.Aeson                                 as Json
import qualified Data.ByteString.Lazy                       as BL

import           Control.Exception                          (bracket)
import           Data.Time.Clock                            (getCurrentTime)
import           System.Directory                           (listDirectory)
import           Data.FileEmbed                             (embedStringFile, makeRelativeToProject)


-- |
runWithDb
    :: -- | Action to perform each time after storing a list of 'CD.ABook'
       (String -> [CD.ABook] -> Postgres.Connection -> IO ())
       -- | Action to perform after all 'CD.ABook's have been stored
    -> ([([CD.ABook], String)] -> Postgres.Connection -> IO a)
    -> IO a
runWithDb afterSingleStore afterAllStore = do
    booksList <- mapM decodeFileOrFail =<< getTestFiles
    withPreparedDb $ \conn -> do
        forM_ booksList $ \(books, file) -> do
            _ <- mainStore conn books
            afterSingleStore file books conn
        afterAllStore booksList conn
  where
    throwError file str = error $ file ++ ": " ++ str
    decodeFileOrFail file = do
        books <- either (throwError file) return =<< Json.eitherDecodeFileStrict file
        return (books, file)

testJsonData :: [BL.ByteString]
testJsonData =
    [ $(makeRelativeToProject "test/data/test.json.zip" >>= embedStringFile)
    , $(makeRelativeToProject "test/data/test2.json.zip" >>= embedStringFile)
    ]

getTestFiles :: IO [FilePath]
getTestFiles = do
    jsonFiles <- filter jsonExtension <$> listDirectory testDataDir
    return $ map (testDataDir ++) jsonFiles
  where
    testDataDir = "test/data/"
    jsonExtension fileName = let splitByDot = T.split (== '.') (toS fileName) in
        if null splitByDot
            then False
            else last splitByDot == "json"

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
