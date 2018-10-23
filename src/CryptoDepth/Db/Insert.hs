module CryptoDepth.Db.Insert
( insertAll
, liquidPathsMap
)
where

import CryptoDepth.Db.Internal.Prelude
import CryptoDepth.Db.Insert.Run        (storeRun, RunId)
import CryptoDepth.Db.Insert.Path       (storePaths)
import CryptoDepth.Db.Insert.Book       (storeBooks)
import CryptoDepth.Db.Insert.RunSymbol  (storeRunSymbols)

import qualified CryptoDepth            as CD
import Database.Beam.Postgres           (Pg)
import Data.Time.LocalTime              (TimeZone(..), utcToLocalTime)
import qualified Data.HashMap.Strict    as Map


-- |
type OnePercent = CD.OneDiv 100

insertAll
    :: UTCTime
    -> [CD.ABook]
    -> Pg [CD.Sym]
insertAll time books = do
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

-- TODO: move somewhere else
liquidPathsMap
    :: KnownSymbol numeraire
    => [CD.ABook]
    -> CD.Map CD.Sym (CD.LiquidPaths numeraire OnePercent)
liquidPathsMap books =
    let (graph, rateMap, nodeMap) = CD.buildDepthGraph books
    in CD.symLiquidPaths rateMap nodeMap graph

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
