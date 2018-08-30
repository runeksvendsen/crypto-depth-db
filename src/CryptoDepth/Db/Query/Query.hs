{-# LANGUAGE PartialTypeSignatures #-}
module CryptoDepth.Db.Query.Query
( newestPathsQ
)
where

import CryptoDepth.Db.Internal.Prelude
import CryptoDepth.Db
import CryptoDepth.Db.Table.Run
import CryptoDepth.Db.Table.Path

import Database.Beam
import Database.Beam.Postgres
import Database.Beam.Postgres.Syntax        (PgExpressionSyntax)



newestPathsQ
    :: (KnownSymbol numeraire, PathTable numeraire Postgres)
    => Q PgSelectSyntax CryptoDepthDb s (PathT numeraire (QExpr PgExpressionSyntax s))
newestPathsQ = do
    run <- newestRunQ
    path <- allPathsQ
    guard_ (_pathRunId path ==. pk run)
    return path


pathList
    :: (KnownSymbol numeraire, PathTable numeraire Postgres)
    => Pg [Path numeraire]
pathList = runSelectReturningList selectAllPaths

selectAllPaths
    :: (KnownSymbol numeraire, PathTable numeraire Postgres)
    => SqlSelect PgSelectSyntax (QExprToIdentity (PathT numeraire (QExpr PgSelectSyntax s)))
selectAllPaths = select allPathsQ

allPathsQ
    :: forall numeraire s.
       (KnownSymbol numeraire, PathTable numeraire Postgres)
    => Q PgSelectSyntax CryptoDepthDb s ((PathT numeraire) (QExpr PgExpressionSyntax s))
allPathsQ = all_ (pathTable cryptoDepthDb :: PathEntityType Postgres numeraire)


newestRun :: Pg (Maybe Run)
newestRun = runSelectReturningOne $ select newestRunQ

newestRunQ :: Q PgSelectSyntax CryptoDepthDb s (RunT (QGenExpr QValueContext PgExpressionSyntax s))
newestRunQ =
    limit_ 1 sortedRuns

sortedRuns :: Q PgSelectSyntax CryptoDepthDb s (RunT (QGenExpr QValueContext PgExpressionSyntax s))
sortedRuns =
    orderBy_ (desc_ . _runId) allRuns

allRuns
    :: Q PgSelectSyntax CryptoDepthDb s (RunT (QExpr PgExpressionSyntax s))
allRuns = all_ (_runInfo cryptoDepthDb)
