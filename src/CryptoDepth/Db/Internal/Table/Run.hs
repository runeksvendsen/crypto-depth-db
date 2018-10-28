module CryptoDepth.Db.Internal.Table.Run
( RunT(..)
, Run, RunId
, PrimaryKey(RunId)
-- * Re-exports
, LocalTime
, Word32
)
where

import CryptoDepth.Db.Internal.Prelude
import Database.Beam
import Database.Beam.Backend.SQL.Types  (SqlSerial)
import Data.Word                        (Word32)
import Data.Time.LocalTime              (LocalTime)


data RunT f
    = Run
    { _runId    :: Columnar f (SqlSerial Word32)
    , _runTime  :: Columnar f LocalTime
    } deriving Generic

deriving instance Show (PrimaryKey RunT Identity)
deriving instance Eq (PrimaryKey RunT Identity)

type Run = RunT Identity
type RunId = PrimaryKey RunT Identity

deriving instance Show Run
deriving instance Eq Run

instance Beamable RunT

instance Table RunT where
    data PrimaryKey RunT f = RunId (Columnar f (SqlSerial Word32)) deriving Generic
    primaryKey = RunId . _runId

instance Beamable (PrimaryKey RunT)
