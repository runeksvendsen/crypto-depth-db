{-# LANGUAGE UndecidableInstances #-}
module CryptoDepth.Db.Internal.Table.RunSymbol
( module CryptoDepth.Db.Internal.Table.RunSymbol
, RunT, Run, RunId
-- * Re-exports
, Vector
)
where

import OrderBook.Types                      (SomeOrder)
import CryptoDepth.Db.Internal.Prelude
import CryptoDepth.Db.Internal.Table.Run    (RunT, Run, RunId)
import CryptoDepth.Db.Orphans               ()
import CryptoDepth                          (Sym, OneDiv)

import Database.Beam
import Data.Time.LocalTime                  ()
import qualified Money
import Data.Tagged                          (Tagged)


-- | All the symbols in a given run
data RunSymbolT f
    = RunSymbol
    { _symbolRun    :: PrimaryKey RunT f
    , _symbolSym    :: Columnar f Sym
    } deriving Generic

type RunSymbol = RunSymbolT Identity
type SymbolId = PrimaryKey RunSymbolT Identity

deriving instance Show RunSymbol
deriving instance Eq RunSymbol

instance Beamable RunSymbolT

instance Table RunSymbolT where
    data PrimaryKey RunSymbolT f = SymbolId
        (PrimaryKey RunT f)
        (Columnar f Sym)
            deriving Generic
    primaryKey RunSymbol{..} = SymbolId _symbolRun _symbolSym

instance Beamable (PrimaryKey RunSymbolT)
