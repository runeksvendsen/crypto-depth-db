module CryptoDepth.Db where

import Database.Beam
import CryptoDepth.Db.Table.Path    (PathT)
import CryptoDepth.Db.Table.Run     (RunT)
import CryptoDepth.Db.Table.Book    (BookT)


data CryptoDepthDb f = CryptoDepthDb
    { _runInfo :: f (TableEntity RunT)
    , _books   :: f (TableEntity BookT)
    , _pathUSD :: f (TableEntity (PathT "USD"))
    , _pathEUR :: f (TableEntity (PathT "EUR"))
    , _pathGBP :: f (TableEntity (PathT "GBP"))
    , _pathJPY :: f (TableEntity (PathT "JPY"))
    } deriving Generic

instance Database be CryptoDepthDb

cryptoDepthDb :: DatabaseSettings be CryptoDepthDb
cryptoDepthDb = defaultDbSettings

class PathTable numeraire be where
    pathTable :: CryptoDepthDb (DatabaseEntity be CryptoDepthDb)
              -> DatabaseEntity be CryptoDepthDb (TableEntity (PathT numeraire))

instance PathTable "USD" be where
    pathTable = _pathUSD

instance PathTable "EUR" be where
    pathTable = _pathEUR

instance PathTable "GBP" be where
    pathTable = _pathGBP

instance PathTable "JPY" be where
    pathTable = _pathJPY
