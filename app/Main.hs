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
import qualified Control.Monad.Parallel             as Par
import qualified Data.Text                          as T

import Data.Time.Clock                              (getCurrentTime)
import Text.Printf                                  (printf)
import Control.Monad.IO.Class                       (liftIO)
import Database.PostgreSQL.Simple.Transaction       (withTransaction)
import System.Environment                           (lookupEnv)
-- TEST
import qualified CryptoDepth.Db.Query.Query         as Test
import CryptoDepth.Db.Query


maxRetries = 10
logLevel = Log.LevelDebug
dbEnvVar = "DB_URL"

main :: IO ()
main = do
    let errMsg = "ERROR: " ++ show dbEnvVar ++ " doesn't contain database URL"
    dbUrl <- fromMaybe (error errMsg) <$> lookupEnv dbEnvVar
    Postgres.connectPostgreSQL (toS dbUrl) >>= mainQuery

mainQuery
    :: Postgres.Connection
    -> IO ()
mainQuery conn =
    Beam.withDatabaseDebug putStrLn conn testNewestPathSumsSelect_5
        >>= printUSD
  where
    printUSD :: [( Test.Sym
                 , Test.SlippageQty (Test.OneDiv 20) "USD"
                 , Test.SlippageQty (Test.OneDiv 20) "USD"
                 )]
             -> IO ()
    printUSD = print

mainStore
    :: Postgres.Connection
    -> IO ()
mainStore conn = do
    time <- getCurrentTime
    books <- fetchBooks
    withTransaction conn $
        Beam.withDatabaseDebug putStrLn conn $
            Insert.insertAll time books

fetchBooks :: IO [CD.ABook]
fetchBooks = withLogging $ do
    man <- HTTP.newManager HTTPS.tlsManagerSettings
    let throwErrM ioA = ioA >>= either (error . show) return
    throwErrM $ AppM.runAppM man maxRetries allBooks

withLogging :: IO a -> IO a
withLogging ioa = Log.withStderrLogging $ do
    Log.setLogLevel logLevel
    Log.setLogTimeFormat "%T.%3q"
    ioa

-- | Fetch books, in parallel, from all venues
allBooks :: AppM.AppM IO [CD.ABook]
allBooks =
   concat <$> Par.forM Venues.allVenues fetchVenueBooks

-- | Fetch books from a single venue
fetchVenueBooks
   :: AnyVenue
   -> AppM.AppM IO [CD.ABook]
fetchVenueBooks (AnyVenue p) = do
    allMarkets :: [Market venue] <- EnumMarkets.marketList p
    let marketName = symbolVal (Proxy :: Proxy venue)
    liftIO . Log.log' $ T.pack (printf "%s: %d markets" marketName (length allMarkets) :: String)
    map CD.toABook <$> mapM fetchMarketBook allMarkets
