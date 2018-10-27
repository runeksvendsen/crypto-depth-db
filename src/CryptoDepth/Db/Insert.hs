module CryptoDepth.Db.Insert
( insertAll
-- * Re-exports
, PGTransaction
, runPGTransactionT
)
where

import CryptoDepth.Db.Internal.Prelude
import CryptoDepth.Db.Internal.Insert.Run        (storeRun, RunId)
import CryptoDepth.Db.Internal.Insert.Path       (storePaths)
import CryptoDepth.Db.Internal.Insert.Book       (storeBooks)
import CryptoDepth.Db.Internal.Insert.RunSymbol  (storeRunSymbols)
import CryptoDepth.Db.Internal.Util     (liquidPathsMap)

import qualified CryptoDepth            as CD
import Database.Beam.Postgres           (Pg, Connection)
import Database.PostgreSQL.Transaction  (PGTransaction, runPGTransactionT)
import Data.Time.LocalTime              (TimeZone(..), utcToLocalTime)
import qualified Data.HashMap.Strict    as Map


insertAll
    :: (Pg [CD.Sym] -> IO [CD.Sym]) -- ^ Beam.withDatabase or withDatabaseDebug
    -> UTCTime
    -> [CD.ABook]
    -> PGTransaction [CD.Sym]
insertAll runPg time books =
    liftIO . runPg $ insertAllPg time books

insertAllPg
    :: UTCTime
    -> [CD.ABook]
    -> Pg [CD.Sym]
insertAllPg time books = do
    runId <- storeRun (utcToLocalTime gmt time)
    storeBooks runId books
    symbols <- insertPaths
        runId
        (liquidPathsMap books)
        (liquidPathsMap books)
        (liquidPathsMap books)
        (liquidPathsMap books)
    storeRunSymbols runId symbols
    return symbols
  where
    -- TODO: Use UTCTime somehow, rather than LocalTime@GMT?
    gmt :: TimeZone
    gmt = TimeZone
        { timeZoneMinutes = 0
        , timeZoneSummerOnly = False
        , timeZoneName = "GMT"
        }

-- | Insert paths for all numeraires, and return a list of
--    all distinct symbols
insertPaths
    :: RunId
    -> CD.Map CD.Sym (CD.LiquidPaths "USD" slippageEdgeWeight)
    -> CD.Map CD.Sym (CD.LiquidPaths "EUR" slippageEdgeWeight)
    -> CD.Map CD.Sym (CD.LiquidPaths "GBP" slippageEdgeWeight)
    -> CD.Map CD.Sym (CD.LiquidPaths "JPY" slippageEdgeWeight)
    -> Pg [CD.Sym]
insertPaths _runId usd eur gbp jpy = do
    storePaths _runId usd
    storePaths _runId eur
    storePaths _runId gbp
    storePaths _runId jpy
    return . Map.keys $
        rmVal usd `Map.union`
        rmVal eur `Map.union`
        rmVal gbp `Map.union`
        rmVal jpy
  where
    rmVal = fmap (const ())
