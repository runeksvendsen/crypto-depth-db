module Main where

import           CryptoDepth.Db.Internal.Prelude
import           Orphans
import           CryptoDepth.Db.Internal.Orphans
import qualified CryptoDepth.Db.Internal.Migrate.Run        as Run

import qualified CryptoDepth                                as CD
import qualified CryptoDepth.Exchange                       as CD
import qualified CryptoDepth.Db.Insert                      as Insert

import           CryptoVenues.Types.Market
import           CryptoVenues.Fetch.MarketBook

import qualified Data.HashMap.Strict                        as Map
import qualified Data.HashSet                               as Set
import qualified Database.Beam                              as Beam
import qualified Database.Beam.Postgres                     as Postgres

import qualified Data.Text                                  as T
import qualified Money
import           Test.Hspec

import           Control.Exception                          (bracket)
import           Database.PostgreSQL.Simple.Transaction     (withTransaction)
import           System.Environment                         (lookupEnv)
import           Data.Time.Clock                            (getCurrentTime)
-- TEST
import qualified CryptoDepth.Db.Query                       as Test
import qualified Data.Aeson                                 as Json
import Data.List                                            (sort, sortOn)


{-
    export DB_URL="host=localhost port=5432 sslmode=disable user=test password=test dbname=test connect_timeout=10"

    docker run -p 5432:5432 --name postgres-test -e POSTGRES_PASSWORD=test -e POSTGRES_USER=test -e POSTGRES_DB=test -d postgres:9.6
-}

main :: IO ()
main = do
    books <- either error return =<<
        Json.eitherDecodeFileStrict "test/data/test.json"
    withPreparedDb (\conn -> mainStore conn books >> hspecMain books conn)

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

openConn :: IO Postgres.Connection
openConn = do
    dbUrl <- dbUrlFromEnvVar    -- TODO: Read from test/config/db.cfg
    Postgres.connectPostgreSQL (toS dbUrl)
  where
    dbUrlFromEnvVar =
        fromMaybe (error errMsg) <$> lookupEnv dbEnvVar
    dbEnvVar = "DB_URL"
    errMsg = "ERROR: " ++ show dbEnvVar ++ " doesn't contain database URL"

hspecMain
    :: [CD.ABook]
    -> Postgres.Connection
    -> IO ()
hspecMain books conn = hspec $ do
    describe "newest path sums" $ do
        it "returns correct sums for 5% slippage" $
            testSumSelect allPathsInfos conn
    describe "newest paths" $
        mapM_ (testPathSelect' conn) (Map.toList allPathsInfos)
  where
    testPathSelect' conn (sym, pathInfos) =
        it ("returns correct PathInfos for " ++ toS sym) $
            testPathSelect conn sym pathInfos
    allPathsInfos = CD.allPathsInfos
        . Insert.liquidPathsMap $ books

type TestSlippage = Test.OneDiv 20
type TestNumeraire = "USD"

testPathSelect
    :: Postgres.Connection
    -> CD.Sym
    -> ( [CD.PathInfo TestNumeraire TestSlippage]
       , [CD.PathInfo TestNumeraire TestSlippage]
       )
    -> Expectation
testPathSelect conn sym pathsInfos = do
    result <- Beam.withDatabase conn (Test.newestBuySellPaths sym)
    mkSets result `shouldBe` mkSets pathsInfos
  where
    mkSets :: ( [CD.PathInfo num slip]
              , [CD.PathInfo num slip]
              )
           -> ( ResultSet num slip
              , ResultSet num slip
              )
    mkSets (a,b) = (ResultSet $ Set.fromList a, ResultSet $ Set.fromList b)

testSumSelect
    :: Map.HashMap
        CD.Sym
        ( [CD.PathInfo TestNumeraire TestSlippage]
        , [CD.PathInfo TestNumeraire TestSlippage]
        )
    -> Postgres.Connection
    -> Expectation
testSumSelect allPathsInfos conn =
    Beam.withDatabase conn Test.testNewestPathSumsSelect_5
        >>= assertEquals
  where
    assertEquals
        :: Map.HashMap
                Test.Sym
                ( Test.SlippageQty TestSlippage TestNumeraire
                , Test.SlippageQty TestSlippage TestNumeraire
                )
        -> IO ()
    assertEquals input =
        mkResultMap input
        `shouldBe`
        mkResultMap (fmap pathTotals allPathsInfos)
    pathTotals (buyL, sellL) =
        ( sum $ map CD.piQty buyL
        , sum $ map CD.piQty sellL
        )

newtype ResultMap currency = ResultMap
    (Map.HashMap Test.Sym (CD.Amount currency, CD.Amount currency))
        deriving Eq

instance KnownSymbol currency => Show (ResultMap currency) where
    show (ResultMap hm) = unlines . fmap show . sortOn fst $ Map.toList hm

mkResultMap ::
    Map.HashMap
        Test.Sym
        ( Test.SlippageQty slippage numeraire
        , Test.SlippageQty slippage numeraire
        )
    -> ResultMap numeraire
mkResultMap =
    ResultMap . fmap (mapTuple unTagged)
  where
    mapTuple f (a,b) = (f a, f b)

newtype ResultSet currency slippage = ResultSet
    (Set.HashSet (CD.PathInfo currency slippage))
        deriving Eq

instance KnownSymbol currency => Show (ResultSet currency slippage) where
    show (ResultSet hs) = unlines . fmap show . sort $ Set.toList hs

mainStore
    :: Postgres.Connection
    -> [CD.ABook]
    -> IO [CD.Sym]
mainStore conn books = do
    time <- getCurrentTime
    -- TODO: 'withTransaction' should be part of library
    withTransaction conn $
        Beam.withDatabase conn $
            Insert.insertAll time books
