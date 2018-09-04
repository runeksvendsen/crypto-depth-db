module CryptoDepth.Db.Query where

import CryptoDepth.Db.Query.Query
import Database.Beam.Query
import Database.Beam.Postgres           (Pg, Postgres)


testNewestPathSumsSelect_5
    :: ( KnownSymbol numeraire
       , PathTable numeraire Postgres
    ) => Pg [(Sym, SlippageQty (OneDiv 20) numeraire, SlippageQty (OneDiv 20) numeraire)]
testNewestPathSumsSelect_5 =
    runSelectReturningList newestPathSumsSelect_5
