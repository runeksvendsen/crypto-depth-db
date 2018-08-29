{-# LANGUAGE PartialTypeSignatures #-}
module CryptoDepth.Db.Query.Query
(
)
where

import CryptoDepth.Db.Internal.Prelude
import CryptoDepth.Db
import CryptoDepth.Db.Table.Run
import CryptoDepth.Db.Table.Path

import Database.Beam
import Database.Beam.Postgres
import Database.Beam.Postgres.Syntax        (PgExpressionSyntax)


allRuns
    :: Q PgSelectSyntax CryptoDepthDb s (RunT (QExpr PgExpressionSyntax s))
allRuns = all_ (_runInfo cryptoDepthDb)

selectSortedRuns
    :: SqlSelect PgSelectSyntax Run
selectSortedRuns =
    select sortBy
  where
    sortBy = orderBy_ (desc_ . _runId) allRuns

newestRun =
    limit_ 1 selectSortedRuns


allPaths
    :: forall numeraire s.
       (KnownSymbol numeraire, PathTable numeraire Postgres)
    => Q PgSelectSyntax CryptoDepthDb s ((PathT numeraire) (QExpr PgExpressionSyntax s))
allPaths = all_ (pathTable cryptoDepthDb :: PathEntityType Postgres numeraire)

usdPaths
    :: (KnownSymbol numeraire, PathTable numeraire Postgres)
    => Pg [Path numeraire]
usdPaths = runSelectReturningList selectAllPaths

selectAllPaths
    :: (KnownSymbol numeraire, PathTable numeraire Postgres)
    => SqlSelect PgSelectSyntax (QExprToIdentity (PathT numeraire (QExpr PgSelectSyntax s)))
selectAllPaths = select allPaths
