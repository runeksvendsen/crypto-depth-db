module CryptoDepth.Db.Insert.Run
( storeRun
, RunId
-- * Re-exports
, LocalTime
)
where

import CryptoDepth.Db.Internal.Prelude
import CryptoDepth.Db.Internal.Table.Run
import qualified CryptoDepth.Db                     as Db

import qualified Database.Beam                      as Beam
import Database.Beam.Postgres                       (Pg)
import Database.Beam.Backend.SQL.BeamExtensions     (runInsertReturningList)


storeRun :: LocalTime -> Pg RunId
storeRun time = fmap (Beam.pk . getSingleResult) $
    runInsertReturningList (Db._runInfo Db.cryptoDepthDb) $
        Beam.insertExpressions [Run Beam.default_ (Beam.val_ time)]
  where
    getSingleResult lst =
        fromMaybe (error $ "storeRun: single item not returned: " ++ show lst )
            $ headMay lst
