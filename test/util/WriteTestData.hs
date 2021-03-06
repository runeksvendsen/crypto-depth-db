module Main where

import qualified CryptoVenues.Fetch.Debug       as Fetch
import qualified CryptoVenues.Types.AppM        as AppM

import           Data.Proxy                     (Proxy(..))
import qualified Network.HTTP.Client.TLS        as HTTPS
import qualified Network.HTTP.Client            as HTTP
import qualified Control.Logging                as Log
import           Options.Applicative
import qualified Data.Aeson                     as Json
import           Control.Error                  (lefts, rights)
import           Control.Monad                  (forM_)


-- | In which currency do we want to measure liquidity?
type Numeraire = "USD"

logLevel = Log.LevelInfo
maxRetries = 15

main :: IO ()
main = do
    args <- execParser opts
    man <- HTTP.newManager HTTPS.tlsManagerSettings
    booksE <- throwErrM $ withLogging $ AppM.runAppM man maxRetries $
        Fetch.allBooks (Proxy :: Proxy Numeraire) (obCount args)
    -- Log errors
    forM_ (lefts booksE) (\err -> putStrLn $ "Errors: " ++ show err)
    -- Write JSON books
    let books = rights booksE
    if books /= []
        then do
            Json.encodeFile (targetFile args) books
            putStrLn $ "Wrote " ++ show (targetFile args)
        else
            putStrLn $ "Fatal error: no order books fetched (all errored)"
  where
    throwErrM ioA = ioA >>= either (error . show) return

withLogging :: IO a -> IO a
withLogging ioa = Log.withStderrLogging $ do
    Log.setLogLevel logLevel
    Log.setLogTimeFormat "%T.%3q"
    ioa

opts :: ParserInfo Options
opts = info options $
     fullDesc
  <> progDesc "Write test data to specified file"
  <> header "Fetch & write test data"

data Options = Options
  { targetFile  :: FilePath
  , obCount     :: Word
  }

options :: Parser Options
options = Options
      <$> targetFile'
      <*> obCount'

targetFile' :: Parser String
targetFile' = strOption
  (  long "file"
  <> metavar "FILEPATH"
  <> help "File path to write test data to" )

obCount' :: Parser Word
obCount' = option auto
  ( long "ob-count"
  <> short 'c'
  <> value 100000
  <> metavar "ORDERBOOK_COUNT"
  <> help "Limit the number of fetched order books" )
