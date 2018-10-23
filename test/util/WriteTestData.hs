module Main where

import           CryptoDepth.Db.Internal.Orphans
import           Data.Proxy                     (Proxy(..))

import qualified CryptoDepth
import qualified CryptoDepth.Fetch              as Fetch
import qualified OrderBook.Types                as OB

import           CryptoVenues.Types.Market
import           CryptoVenues.Fetch.MarketBook
import qualified CryptoVenues.Types.AppM        as AppM

import qualified Network.HTTP.Client.TLS        as HTTPS
import qualified Network.HTTP.Client            as HTTP
import qualified Control.Logging                as Log
import           Options.Applicative
import qualified Data.Aeson                     as Json
import           Data.Aeson                     ((.=), (.:))
import           Data.Text.Lazy.IO              as L
import           System.IO                      ( Handle, IOMode(WriteMode)
                                                , withFile, stderr, hSetEncoding, utf8
                                                )
import Data.Text.Lazy.IO                        as L
import qualified Money
-- TODO: move
import GHC.TypeLits                             (KnownSymbol, SomeSymbol(..)
                                                , symbolVal, someSymbolVal, sameSymbol
                                                )
import Data.Type.Equality                       ((:~:)(Refl))
import Data.Maybe                               (fromMaybe)



-- | In which currency do we want to measure liquidity?
type Numeraire = "USD"

-- DEBUG: How many orderbooks to fetch from each venue
--  (not used in production)
numObLimit :: Word
numObLimit = 10000

logLevel = Log.LevelInfo
maxRetries = 15

main :: IO ()
main = do
    targetFile <- execParser opts
    man <- HTTP.newManager HTTPS.tlsManagerSettings
    let throwErrM ioA = ioA >>= either (error . show) return
    books <- throwErrM $ withLogging $ AppM.runAppM man maxRetries $
        Fetch.allBooks (Proxy :: Proxy Numeraire) numObLimit
    Json.encodeFile targetFile books

withLogging :: IO a -> IO a
withLogging ioa = Log.withStderrLogging $ do
    Log.setLogLevel logLevel
    Log.setLogTimeFormat "%T.%3q"
    ioa

targetFile :: Parser String
targetFile = strOption
  (  long "file"
  <> metavar "FILEPATH"
  <> help "File path to write test data to" )

opts :: ParserInfo String
opts = info targetFile $
     fullDesc
  <> progDesc "Write test data to specified file"
  <> header "Fetch & write test data"
