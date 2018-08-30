module CryptoDepth.Db.Insert.Book
( storeBooks
)
where

import CryptoDepth.Db.Internal.Prelude
import qualified CryptoDepth.Db         as Db
import CryptoDepth.Db.Internal.Table.Book        (Book, BookT(..), RunId)
import qualified CryptoDepth            as CD
import qualified Database.Beam          as Beam
import Database.Beam.Postgres           (Pg)
import OrderBook.Types                  (OrderBook, someBookOrders, toSomeBook)


storeBooks
    :: RunId
    -> [CD.ABook]
    -> Pg ()
storeBooks runId books = Beam.runInsert $
    Beam.insert (Db._books Db.cryptoDepthDb) $
        Beam.insertValues $ map toBook books
  where
    toBook :: CD.ABook
           -> Book
    toBook (CD.ABook ob) = fromOB ob
    fromOB :: forall venue base quote.
              (KnownSymbol venue, KnownSymbol base, KnownSymbol quote)
           => OrderBook venue base quote
           -> Book
    fromOB ob =
        let (bids, asks) = someBookOrders (toSomeBook ob) in
        Book
        { _bookRunId    = runId
        , _bookVenue    = toS $ symbolVal (Proxy :: Proxy venue)
        , _bookBase     = toS $ symbolVal (Proxy :: Proxy base)
        , _bookQuote    = toS $ symbolVal (Proxy :: Proxy quote)
        , _bookBids     = bids
        , _bookAsks     = asks
        }
