{-# LANGUAGE UndecidableInstances #-}
module CryptoDepth.Db.Internal.Table.Book
( module CryptoDepth.Db.Internal.Table.Book
, RunT, Run, RunId
)
where

import OrderBook.Types                  (SomeOrder)
import CryptoDepth.Db.Internal.Prelude
import CryptoDepth.Db.Internal.Table.Run         (RunT, Run, RunId)
import CryptoDepth.Db.Orphans           ()
import CryptoDepth                      (Sym, OneDiv)

import Database.Beam
import GHC.TypeLits                     (KnownSymbol, Symbol)
import Data.Time.LocalTime              ()
import qualified Money
import Data.Tagged                      (Tagged)


type Venue = Text
type Base = Text
type Quote = Text

data BookT f
    = Book
    { _bookRunId    :: PrimaryKey RunT f
    , _bookVenue    :: Columnar f Venue
    , _bookBase     :: Columnar f Base
    , _bookQuote    :: Columnar f Quote
    , _bookBids     :: Columnar f (Vector SomeOrder)
    , _bookAsks     :: Columnar f (Vector SomeOrder)
    } deriving Generic

type Book = BookT Identity
type BookId = PrimaryKey BookT Identity

deriving instance Show Book
deriving instance Eq Book

instance Beamable BookT

instance Table BookT where
    data PrimaryKey BookT f = BookId
        (PrimaryKey RunT f)
        (Columnar f Venue)
        (Columnar f Base)
        (Columnar f Quote)
            deriving Generic
    primaryKey Book{..} = BookId _bookRunId _bookVenue _bookBase _bookQuote

instance Beamable (PrimaryKey BookT)
