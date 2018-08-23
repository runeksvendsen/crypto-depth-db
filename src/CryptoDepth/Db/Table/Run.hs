module CryptoDepth.Db.Table.Run
( RunT(Run)
, Run, RunId
, UTCTime
)
where

import CryptoDepth.Db.Internal.Prelude
import Database.Beam
import Data.Word                        (Word32)
import Data.Time.LocalTime              ()


data RunT f
    = Run
    { _runId    :: Columnar f Word32
    , _runTime  :: Columnar f UTCTime
    } deriving Generic

deriving instance Show (PrimaryKey RunT Identity)
deriving instance Eq (PrimaryKey RunT Identity)

type Run = RunT Identity
type RunId = PrimaryKey RunT Identity

deriving instance Show Run
deriving instance Eq Run

instance Beamable RunT

instance Table RunT where
    data PrimaryKey RunT f = RunId (Columnar f Word32) deriving Generic
    primaryKey = RunId . _runId

instance Beamable (PrimaryKey RunT)
