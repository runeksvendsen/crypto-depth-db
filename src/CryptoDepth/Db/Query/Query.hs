{-# LANGUAGE PartialTypeSignatures #-}
module CryptoDepth.Db.Query.Query
( newestPathsQ
)
where

import CryptoDepth.Db.Internal.Prelude
import CryptoDepth.Db
import CryptoDepth.Db.Internal.Table.Run
import CryptoDepth.Db.Internal.Table.Path

import Database.Beam
import Database.Beam.Postgres
import Database.Beam.Postgres.Syntax        (PgExpressionSyntax)


newestPathsQ
    :: (KnownSymbol numeraire, PathTable numeraire Postgres)
    => Q PgSelectSyntax CryptoDepthDb s (PathT numeraire (QExpr PgExpressionSyntax s))
newestPathsQ = do
    newestRun <- newestRunQ
    path <- allPathsQ
    guard_ (_pathRunId path `references_` newestRun)
    return path

allPathsQ
    :: forall numeraire s.
       (KnownSymbol numeraire, PathTable numeraire Postgres)
    => Q PgSelectSyntax CryptoDepthDb s ((PathT numeraire) (QExpr PgExpressionSyntax s))
allPathsQ = all_ (pathTable cryptoDepthDb :: PathEntityType Postgres numeraire)


newestRunQ :: Q PgSelectSyntax CryptoDepthDb s (RunT (QGenExpr QValueContext PgExpressionSyntax s))
newestRunQ =
    limit_ 1 sortedRuns

sortedRuns :: Q PgSelectSyntax CryptoDepthDb s (RunT (QGenExpr QValueContext PgExpressionSyntax s))
sortedRuns =
    orderBy_ (desc_ . _runId) allRuns

allRuns
    :: Q PgSelectSyntax CryptoDepthDb s (RunT (QExpr PgExpressionSyntax s))
allRuns = all_ (_runInfo cryptoDepthDb)
