{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
module CryptoDepth.Db.Internal.Query.Paths
( newestBuySellPaths
, PathTable
, CD.Sym
, CD.OneDiv
, CD.PathInfo(..)
, KnownSymbol
)
where

import CryptoDepth.Db.Internal.Prelude
import CryptoDepth.Db.Internal.Query.Common
import CryptoDepth.Db
import CryptoDepth.Db.Internal.Table.Run
import CryptoDepth.Db.Internal.Table.Path
import CryptoDepth.Db.Internal.Table.RunSymbol
import CryptoDepth.Db.Orphans                   ()
import qualified CryptoDepth as CD
import qualified Money

import Database.Beam
import Database.Beam.Query.Internal         (QNested)
import Database.Beam.Postgres
import Database.Beam.Postgres.Syntax        (PgExpressionSyntax)


-- |
newestBuySellPaths
    :: ( KnownSymbol numeraire
       , PathTable numeraire Postgres
       , PathQuantity slippage numeraire Identity
       )
    => Sym
    -- | (buyPaths, sellPaths)
    -> Pg ([CD.PathInfo numeraire slippage], [CD.PathInfo numeraire slippage])
newestBuySellPaths sym = do
    buyPaths  <- runSelectReturningList . select $ newestBuyPaths sym
    sellPaths <- runSelectReturningList . select $ newestSellPaths sym
    return (map mkPathInfo buyPaths, map mkPathInfo sellPaths)
  where
    mkPathInfo :: forall numeraire slippage.
                  PathQuantity slippage numeraire Identity
               => Path numeraire
               -> CD.PathInfo numeraire slippage
    mkPathInfo path =
        CD.PathInfo
            (fromC' $ pathQuantity path)
            (_pathPath path)

newestBuyPaths
    :: ( KnownSymbol numeraire
       , PathTable numeraire Postgres
       )
    => CD.Sym
    -> Q PgSelectSyntax CryptoDepthDb s (PathT numeraire (QExpr PgExpressionSyntax s))
newestBuyPaths = newestPathsSym _pathSrc _pathDst

newestSellPaths
    :: forall numeraire s.
       ( KnownSymbol numeraire
       , PathTable numeraire Postgres
       )
    => CD.Sym
    -> Q PgSelectSyntax CryptoDepthDb s (PathT numeraire (QExpr PgExpressionSyntax s))
newestSellPaths = newestPathsSym _pathDst _pathSrc

-- | Return slippage sums for paths from one specified symbol to another.
-- E.g. "newestPathSums isNumeraire groupField" returns the slippage sums
--  for "path"s grouped by "groupField path" (and where "isNumeraire path == numeraire")
newestPathsSym
    :: forall numeraire s.
       ( KnownSymbol numeraire
       , PathTable numeraire Postgres
       )
    => ( PathT numeraire (QExpr PgExpressionSyntax s)
            -> Columnar (QExpr PgExpressionSyntax s) CD.Sym
       )    -- ^ Field that is equal to 'numeraire' symbol
    -> ( PathT numeraire (QExpr PgExpressionSyntax s)
            -> Columnar (QExpr PgExpressionSyntax s) CD.Sym
       )    -- ^ Return paths for this symbol
    -> CD.Sym
    -> Q PgSelectSyntax CryptoDepthDb s (PathT numeraire (QExpr PgExpressionSyntax s))
newestPathsSym isNumeraire symField sym = do
    path <- newestPaths isNumeraire
    guard_ (symField path ==. val_ sym)
    return path
