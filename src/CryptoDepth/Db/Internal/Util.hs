module CryptoDepth.Db.Internal.Util
( liquidPathsMap
)
where

import           CryptoDepth.Db.Internal.Prelude
import qualified CryptoDepth                        as CD


type OnePercent = CD.OneDiv 100

liquidPathsMap
    :: KnownSymbol numeraire
    => [CD.ABook]
    -> CD.Map CD.Sym (CD.LiquidPaths numeraire OnePercent)
liquidPathsMap books =
    let (graph, rateMap, nodeMap) = CD.buildDepthGraph books
    in CD.symLiquidPaths rateMap nodeMap graph
