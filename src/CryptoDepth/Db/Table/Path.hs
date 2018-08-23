{-# LANGUAGE UndecidableInstances #-}
module CryptoDepth.Db.Table.Path
( module CryptoDepth.Db.Table.Path
, RunT, Run, RunId
)
where

import CryptoDepth.Db.Internal.Prelude
import CryptoDepth.Db.Table.Run         (RunT, Run, RunId)
import CryptoDepth.Db.Orphans           ()
import CryptoDepth                      (Sym, OneDiv)

import Database.Beam
import GHC.TypeLits                     (KnownSymbol, Symbol)
import Data.Time.LocalTime              ()
import qualified Money
import Data.Tagged                      (Tagged)


data PathT (numeraire :: Symbol) f
    = Path
    { _pathRunId    :: PrimaryKey RunT f
    , _pathSrc      :: Columnar f Sym
    , _pathDst      :: Columnar f Sym
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
