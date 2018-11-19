module CryptoDepth.Db.Internal.Prelude
( module Protolude.Safe
, module TypeLits
, module Data.Maybe
, Identity
, Word32, Word64, Int64
, SymVenue
, NonEmpty
, Tagged(..)
, UTCTime
, Text
, Vector
, Proxy(..)
, MonadIO, liftIO
, toS
, printf
, module Monad
, putStrLnErr
, fromC'
, trace
)
where

import Protolude                        (toS)
import Protolude.Safe
import GHC.TypeLits as TypeLits
import Data.Functor.Identity            (Identity)
import Data.Word                        (Word32, Word64)
import Data.Int                         (Int64)
-- TODO: export from 'CryptoDepth'
import CryptoDepth.Internal.Types       (SymVenue)
import Data.List.NonEmpty               (NonEmpty)
import Data.Tagged                      (Tagged(..))
import Data.Maybe
import Data.Time.Clock                  (UTCTime)
import Data.Text                        (Text)
import Data.Vector                      (Vector)
import Data.Proxy                       (Proxy(..))
import Text.Printf                      (printf)
import Control.Monad as Monad           ((<=<), (>=>), forM, forM_)
import qualified System.IO              as IO
import Database.Beam.Schema.Tables      (Columnar, Columnar'(..))
import Debug.Trace                      (trace)
import Control.Monad.IO.Class           (MonadIO, liftIO)


putStrLnErr :: String -> IO ()
putStrLnErr = IO.hPutStrLn IO.stderr

fromC' :: Columnar' f a -> Columnar f a
fromC' (Columnar' c) = c
