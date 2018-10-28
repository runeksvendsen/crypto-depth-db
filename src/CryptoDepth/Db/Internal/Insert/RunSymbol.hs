module CryptoDepth.Db.Internal.Insert.RunSymbol
( storeRunSymbols
)
where

import CryptoDepth.Db.Internal.Prelude
import qualified CryptoDepth.Db                 as Db
import CryptoDepth.Db.Internal.Table.RunSymbol
import qualified CryptoDepth                    as CD
import qualified Database.Beam                  as Beam
import Database.Beam.Postgres                   (Pg)
import OrderBook.Types                          (OrderBook, someBookOrders, toSomeBook)


storeRunSymbols
    :: RunId
    -> [CD.Sym]
    -> Pg ()
storeRunSymbols runId symbols = Beam.runInsert $
    Beam.insert (Db._symbols Db.cryptoDepthDb) $
        Beam.insertValues $ map (RunSymbol runId) symbols
