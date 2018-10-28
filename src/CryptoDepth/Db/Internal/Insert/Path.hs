module CryptoDepth.Db.Internal.Insert.Path
( storePaths
, RunId
)
where

import CryptoDepth.Db.Internal.Prelude
import qualified CryptoDepth.Db         as Db
import CryptoDepth.Db.Internal.Table.Path        (Path, PathT(..), RunId)
import qualified CryptoDepth            as CD
import qualified Data.HashMap.Strict    as Map
import qualified Database.Beam          as Beam
import Database.Beam.Postgres           (Pg, Postgres)


storePaths
    :: forall numeraire slippageEdgeWeight.
       (KnownSymbol numeraire, Db.PathTable numeraire Postgres)
    => RunId
    -> CD.Map CD.Sym (CD.LiquidPaths numeraire slippageEdgeWeight)
    -> Pg ()
storePaths _runId paths = Beam.runInsert $
    Beam.insert (Db.pathTable Db.cryptoDepthDb :: Db.PathEntityType Postgres numeraire) $
        Beam.insertValues $ concatMap (toPaths . snd) (Map.toList paths)
  where
    toPaths :: CD.LiquidPaths numeraire slippageEdgeWeight
            -> [Path numeraire]
    toPaths (CD.LiquidPaths buyPaths sellPaths) =
        map toPath buyPaths ++ map toPath sellPaths
    toPath :: CD.EdgePath numeraire
           -> Path numeraire
    toPath ep = Path
        { _pathRun    = _runId
        , _pathSrc      = CD.srcSym ep
        , _pathDst      = CD.dstSym ep
        , _pathPath     = CD.pathDescr ep
        , _pathQty01   = CD.pathQty ep
        , _pathQty05   = CD.pathQty ep
        , _pathQty1    = CD.pathQty ep
        , _pathQty5    = CD.pathQty ep
        }
