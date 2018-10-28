{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
module CryptoDepth.Db.Internal.Query.RunSymbols
( newestSymbolsSelect
, CD.Sym
)
where

import CryptoDepth.Db.Internal.Prelude
import CryptoDepth.Db.Internal.Query.Common
import CryptoDepth.Db
import CryptoDepth.Db.Internal.Table.Run
import CryptoDepth.Db.Internal.Table.Path
import CryptoDepth.Db.Internal.Table.RunSymbol
import qualified CryptoDepth as CD
import qualified Money

import Database.Beam
import Database.Beam.Query.Internal         (QNested)
import Database.Beam.Postgres
import Database.Beam.Postgres.Syntax        (PgExpressionSyntax)


newestSymbolsSelect :: SqlSelect PgSelectSyntax Text
newestSymbolsSelect = select $
    _symbolSym <$> newestSymbols

newestSymbols
    :: Q PgSelectSyntax CryptoDepthDb s (RunSymbolT (QExpr PgExpressionSyntax s))
newestSymbols = do
    run <- newestRun
    runSymbols <- allRunSymbols
    guard_ (_symbolRun runSymbols `references_` run)
    return runSymbols
  where
    allRunSymbols = all_ (_symbols cryptoDepthDb)
