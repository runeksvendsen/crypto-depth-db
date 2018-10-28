module CryptoDepth.Db where

import Database.Beam
import CryptoDepth.Db.Internal.Table.Path    (PathT)
import CryptoDepth.Db.Internal.Table.Run     (RunT)
import CryptoDepth.Db.Internal.Table.Book    (BookT)
import CryptoDepth.Db.Internal.Table.RunSymbol  (RunSymbolT)


data CryptoDepthDb f = CryptoDepthDb
    { _runs      :: f (TableEntity RunT)
    , _books     :: f (TableEntity BookT)
    , _symbols   :: f (TableEntity RunSymbolT)
    , _paths_usd :: f (TableEntity (PathT "USD"))
    , _paths_eur :: f (TableEntity (PathT "EUR"))
    , _paths_gbp :: f (TableEntity (PathT "GBP"))
    , _paths_jpy :: f (TableEntity (PathT "JPY"))
    } deriving Generic

instance Database be CryptoDepthDb

cryptoDepthDb :: DatabaseSettings be CryptoDepthDb
cryptoDepthDb = defaultDbSettings

class PathTable numeraire be where
    pathTable :: CryptoDepthDb (DatabaseEntity be CryptoDepthDb)
              -> DatabaseEntity be CryptoDepthDb (TableEntity (PathT numeraire))

instance PathTable "USD" be where
    pathTable = _paths_usd

instance PathTable "EUR" be where
    pathTable = _paths_eur

instance PathTable "GBP" be where
    pathTable = _paths_gbp

instance PathTable "JPY" be where
    pathTable = _paths_jpy

type PathEntityType be numeraire =
    DatabaseEntity be CryptoDepthDb (TableEntity (PathT numeraire))
