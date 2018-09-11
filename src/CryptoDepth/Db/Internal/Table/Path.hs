{-# LANGUAGE UndecidableInstances #-}
module CryptoDepth.Db.Internal.Table.Path
( module CryptoDepth.Db.Internal.Table.Path
, RunT, Run, RunId
, Columnar'(Columnar')
-- * Re-exports
, SymVenue
, NonEmpty
, Tagged
, Money.Dense
, OneDiv
, KnownSymbol
)
where

import CryptoDepth.Db.Internal.Prelude
import CryptoDepth.Db.Internal.Table.Run         (RunT, Run, RunId)
import CryptoDepth.Db.Orphans           ()
import CryptoDepth                      (Sym, OneDiv)

import Database.Beam
import GHC.TypeLits                     (KnownSymbol, Symbol)
import Data.Time.LocalTime              ()
import qualified Money
import Data.Tagged                      (Tagged)
import Database.Beam.Schema.Tables      (Columnar'(Columnar'))


data PathT (numeraire :: Symbol) f
    = Path
    { _pathRunId    :: PrimaryKey RunT f
    -- | Starting symbol for the path
    , _pathSrc      :: Columnar f Sym
    -- | Ending symbol for the path
    , _pathDst      :: Columnar f Sym
    -- | Actual path
    , _pathPath     :: Columnar f (NonEmpty SymVenue)
    -- | 0.1% slippage
    , _pathQty_01   :: Columnar f (Tagged (OneDiv 1000) (Money.Dense numeraire))
    -- | 0.5% slippage
    , _pathQty_05   :: Columnar f (Tagged (OneDiv 200)  (Money.Dense numeraire))
    -- | 1% slippage
    , _pathQty_1    :: Columnar f (Tagged (OneDiv 100)  (Money.Dense numeraire))
    -- | 5% slippage
    , _pathQty_5    :: Columnar f (Tagged (OneDiv 20)   (Money.Dense numeraire))
    } deriving Generic

type Path numeraire = (PathT numeraire) Identity
type PathId numeraire = PrimaryKey (PathT numeraire) Identity

deriving instance KnownSymbol numeraire => Show (Path numeraire)
deriving instance Eq (Path numeraire)

instance Beamable (PathT numeraire)

instance KnownSymbol numeraire => Table (PathT numeraire) where
    data PrimaryKey (PathT numeraire) f = PathId
        (PrimaryKey RunT f)
        (Columnar f (NonEmpty SymVenue))
            deriving Generic
    primaryKey Path{..} = PathId _pathRunId _pathPath

instance Beamable (PrimaryKey (PathT numeraire))

class PathQuantity slippage numeraire f where
    pathQuantity :: PathT numeraire f
                 -> Columnar' f (Tagged slippage (Money.Dense numeraire))

instance PathQuantity (OneDiv 1000) numeraire f where
    pathQuantity = Columnar' . _pathQty_01

instance PathQuantity (OneDiv 200) numeraire f where
    pathQuantity = Columnar' . _pathQty_05

instance PathQuantity (OneDiv 100) numeraire f where
    pathQuantity = Columnar' . _pathQty_1

instance PathQuantity (OneDiv 20) numeraire f where
    pathQuantity = Columnar' . _pathQty_5
