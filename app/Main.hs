module Main where

import CryptoDepth.Db.Internal.Prelude
import qualified CryptoDepth                        as CD
import qualified CryptoDepth.Db.Insert              as Insert

import CryptoVenues.Types.Market
import CryptoVenues.Fetch.MarketBook
import qualified CryptoVenues.Fetch.EnumMarkets     as EnumMarkets
import qualified CryptoVenues.Venues                as Venues
import qualified CryptoVenues.Types.AppM            as AppM

import qualified Database.Beam                      as Beam
import qualified Database.Beam.Postgres             as Postgres

import qualified Network.HTTP.Client                as HTTP
import qualified Network.HTTP.Client.TLS            as HTTPS
import qualified Control.Logging                    as Log
import qualified Data.Text                          as T

import Data.Time.Clock                              (getCurrentTime)
import Text.Printf                                  (printf)
import Database.PostgreSQL.Simple.Transaction       (withTransaction)
import System.Environment                           (lookupEnv)
-- TEST
import qualified CryptoDepth.Db.Query         as Test
import CryptoDepth.Db.Query


maxRetries = 10
logLevel = Log.LevelDebug
dbEnvVar = "DB_URL"

main :: IO ()
main = do
    let errMsg = "ERROR: " ++ show dbEnvVar ++ " doesn't contain database URL"
    dbUrl <- fromMaybe (error errMsg) <$> lookupEnv dbEnvVar
    Postgres.connectPostgreSQL (toS dbUrl) >>= undefined --mainQuery
