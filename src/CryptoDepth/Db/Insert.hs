module CryptoDepth.Db.Insert
( insertAll
)
where

import CryptoDepth.Db.Internal.Prelude
import CryptoDepth.Db.Insert.Run        (storeRun, RunId)
import CryptoDepth.Db.Insert.Path       (storePaths)
import CryptoDepth.Db.Insert.Book       (storeBooks)

import qualified CryptoDepth            as CD
import Database.Beam.Postgres           (Pg)
import Data.Time.LocalTime              (TimeZone(..), utcToLocalTime)


type OnePercent = CD.OneDiv 100

insertAll
    :: UTCTime
    -> [CD.ABook]
    -> Pg ()
insertAll time books = do
    runId <- storeRun (utcToLocalTime gmt time)
    storeBooks runId books
    insertPaths
        runId
        (liquidPathsMap books)
        (liquidPathsMap books)
        (liquidPathsMap books)
        (liquidPathsMap books)
  where
    gmt :: TimeZone
    gmt = TimeZone
        { timeZoneMinutes = 0
        , timeZoneSummerOnly = False
        , timeZoneName = "GMT"
        }

liquidPathsMap
    :: KnownSymbol numeraire
    => [CD.ABook]
    -> CD.Map CD.Sym (CD.LiquidPaths numeraire OnePercent)
liquidPathsMap books =
    let (graph, rateMap, nodeMap) = CD.buildDepthGraph books
    in CD.symLiquidPaths rateMap nodeMap graph

insertPaths
    :: RunId
    -> CD.Map CD.Sym (CD.LiquidPaths "USD" slippageEdgeWeight)
    -> CD.Map CD.Sym (CD.LiquidPaths "EUR" slippageEdgeWeight)
    -> CD.Map CD.Sym (CD.LiquidPaths "GBP" slippageEdgeWeight)
    -> CD.Map CD.Sym (CD.LiquidPaths "JPY" slippageEdgeWeight)
    -> Pg ()
insertPaths runId usd eur gbp jpy = do
    storePaths runId usd
    storePaths runId eur
    storePaths runId gbp
    storePaths runId jpy
